// GENERATED from a TRACED frame (shape 7). Do not edit.
//
// One input shape, 4 output shapes, 29 distinct button
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
pub const SHAPE: u64 = 7233084974395260882;

/// (path, kind) - resolved against a block at bind time.
pub const UNI_SLOTS: &[(&str, &str)] = &[
    ("objects[0].hitbox.h", "num"),
    ("objects[0].hitbox.w", "num"),
    ("objects[0].hitbox.x", "num"),
    ("objects[0].hitbox.y", "num"),
    ("objects[1].hitbox.h", "num"),
    ("objects[1].hitbox.w", "num"),
    ("objects[1].hitbox.x", "num"),
    ("objects[1].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c294: P8,
    pub c295: P8,
    pub c296: P8,
    pub c297: P8,
    pub c304: P8,
    pub c305: P8,
    pub c306: P8,
    pub c307: P8,
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
    ("objects[0].flip.x", "bool"),
    ("objects[0].flip.y", "bool"),
    ("objects[0].rem.x", "num"),
    ("objects[0].rem.y", "num"),
    ("objects[0].solids", "bool"),
    ("objects[0].spd.x", "num"),
    ("objects[0].spd.y", "num"),
    ("objects[0].spr", "num"),
    ("objects[0].x", "num"),
    ("objects[0].y", "num"),
    ("objects[1].collideable", "bool"),
    ("objects[1].delay", "num"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].spr", "num"),
    ("objects[1].state", "num"),
    ("objects[1].target.x", "num"),
    ("objects[1].target.y", "num"),
    ("objects[1].x", "num"),
    ("objects[1].y", "num"),
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
    pub c234: u16,
    pub c292: u16,
    pub c293: u16,
    pub c298: ZN,
    pub c299: ZN,
    pub c243: u16,
    pub c300: ZN,
    pub c301: ZN,
    pub c245: ZN,
    pub c247: ZN,
    pub c248: ZN,
    pub c251: u16,
    pub c252: ZN,
    pub c302: u16,
    pub c303: u16,
    pub c308: ZN,
    pub c309: ZN,
    pub c261: u16,
    pub c310: ZN,
    pub c311: ZN,
    pub c263: ZN,
    pub c264: ZN,
    pub c312: ZN,
    pub c313: ZN,
    pub c267: ZN,
    pub c268: ZN,
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
    pub c234: u32,
    pub c292: u32,
    pub c293: u32,
    pub c298: u32,
    pub c299: u32,
    pub c243: u32,
    pub c300: u32,
    pub c301: u32,
    pub c245: u32,
    pub c247: u32,
    pub c248: u32,
    pub c251: u32,
    pub c252: u32,
    pub c302: u32,
    pub c303: u32,
    pub c308: u32,
    pub c309: u32,
    pub c261: u32,
    pub c310: u32,
    pub c311: u32,
    pub c263: u32,
    pub c264: u32,
    pub c312: u32,
    pub c313: u32,
    pub c267: u32,
    pub c268: u32,
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
        c294: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c295: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c296: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c297: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c304: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c305: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c306: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c307: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
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
        c234: cell("objects[0].collideable")?,
        c292: cell("objects[0].flip.x")?,
        c293: cell("objects[0].flip.y")?,
        c298: cell("objects[0].rem.x")?,
        c299: cell("objects[0].rem.y")?,
        c243: cell("objects[0].solids")?,
        c300: cell("objects[0].spd.x")?,
        c301: cell("objects[0].spd.y")?,
        c245: cell("objects[0].spr")?,
        c247: cell("objects[0].x")?,
        c248: cell("objects[0].y")?,
        c251: cell("objects[1].collideable")?,
        c252: cell("objects[1].delay")?,
        c302: cell("objects[1].flip.x")?,
        c303: cell("objects[1].flip.y")?,
        c308: cell("objects[1].rem.x")?,
        c309: cell("objects[1].rem.y")?,
        c261: cell("objects[1].solids")?,
        c310: cell("objects[1].spd.x")?,
        c311: cell("objects[1].spd.y")?,
        c263: cell("objects[1].spr")?,
        c264: cell("objects[1].state")?,
        c312: cell("objects[1].target.x")?,
        c313: cell("objects[1].target.y")?,
        c267: cell("objects[1].x")?,
        c268: cell("objects[1].y")?,
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
        c234: match &b.cols[s.c234 as usize] {
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
        c292: match &b.cols[s.c292 as usize] {
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
        c293: match &b.cols[s.c293 as usize] {
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
        c298: match &b.cols[s.c298 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c299: match &b.cols[s.c299 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c243: match &b.cols[s.c243 as usize] {
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
        c300: match &b.cols[s.c300 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c301: match &b.cols[s.c301 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c245: match &b.cols[s.c245 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c247: match &b.cols[s.c247 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c248: match &b.cols[s.c248 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c251: match &b.cols[s.c251 as usize] {
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
        c252: match &b.cols[s.c252 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c302: match &b.cols[s.c302 as usize] {
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
        c303: match &b.cols[s.c303 as usize] {
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
        c308: match &b.cols[s.c308 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c309: match &b.cols[s.c309 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c261: match &b.cols[s.c261 as usize] {
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
        c310: match &b.cols[s.c310 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c311: match &b.cols[s.c311 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c263: match &b.cols[s.c263 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c264: match &b.cols[s.c264 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c312: match &b.cols[s.c312 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c313: match &b.cols[s.c313 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c267: match &b.cols[s.c267 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c268: match &b.cols[s.c268 as usize] {
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (298, "objects[0].flip.x"),
    (299, "objects[0].flip.y"),
    (300, "objects[0].hitbox.h"),
    (301, "objects[0].hitbox.w"),
    (302, "objects[0].hitbox.x"),
    (303, "objects[0].hitbox.y"),
    (304, "objects[0].rem.x"),
    (305, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (306, "objects[0].spd.x"),
    (307, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (247, "objects[0].x"),
    (248, "objects[0].y"),
    (251, "objects[1].collideable"),
    (308, "objects[1].dash_accel.x"),
    (309, "objects[1].dash_accel.y"),
    (253, "objects[1].dash_effect_time"),
    (310, "objects[1].dash_target.x"),
    (311, "objects[1].dash_target.y"),
    (255, "objects[1].dash_time"),
    (256, "objects[1].djump"),
    (312, "objects[1].flip.x"),
    (313, "objects[1].flip.y"),
    (258, "objects[1].grace"),
    (314, "objects[1].hitbox.h"),
    (315, "objects[1].hitbox.w"),
    (316, "objects[1].hitbox.x"),
    (317, "objects[1].hitbox.y"),
    (265, "objects[1].p_dash"),
    (266, "objects[1].p_jump"),
    (318, "objects[1].rem.x"),
    (319, "objects[1].rem.y"),
    (268, "objects[1].solids"),
    (320, "objects[1].spd.x"),
    (321, "objects[1].spd.y"),
    (272, "objects[1].x"),
    (273, "objects[1].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (0, 246), (1, 247), (2, 248)]),
    SCell::Obj(&[(21, 249), (20, 250), (11, 251), (28, 252), (33, 253), (27, 254), (26, 255), (30, 256), (14, 257), (29, 258), (15, 259), (19, 260), (18, 261), (22, 262), (23, 263), (24, 264), (32, 265), (31, 266), (4, 267), (12, 268), (3, 269), (13, 270), (0, 271), (1, 272), (2, 273)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 298), (2, 299)]),
    SCell::Obj(&[(17, 300), (16, 301), (1, 302), (2, 303)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 304), (2, 305)]),
    SCell::Obj(&[(1, 306), (2, 307)]),
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 308), (2, 309)]),
    SCell::Obj(&[(1, 310), (2, 311)]),
    SCell::Obj(&[(1, 312), (2, 313)]),
    SCell::Obj(&[(17, 314), (16, 315), (1, 316), (2, 317)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 318), (2, 319)]),
    SCell::Obj(&[(1, 320), (2, 321)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (152, 206),
    (153, 207),
    (154, 208),
    (155, 209),
    (156, 210),
    (158, 211),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 212),
    (173, 213),
    (174, 214),
    (176, 215),
    (177, 216),
    (179, 217),
    (181, 218),
    (183, 219),
    (185, 220),
    (187, 221),
    (190, 222),
    (193, 223),
    (195, 224),
    (197, 225),
    (198, 226),
    (199, 227),
    (200, 228),
    (201, 229),
    (203, 230),
    (204, 231),
    (232, 274),
    (233, 275),
    (235, 276),
    (236, 277),
    (237, 278),
    (238, 279),
    (239, 280),
    (240, 281),
    (241, 282),
    (242, 283),
    (244, 284),
    (246, 123),
    (249, 285),
    (250, 286),
    (252, 287),
    (254, 288),
    (257, 289),
    (259, 290),
    (260, 291),
    (261, 292),
    (262, 293),
    (263, 294),
    (264, 295),
    (267, 296),
    (269, 297),
    (271, 93),
];

/// Outcome 0's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared0 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c86: ZN,
    pub c273: ZN,
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
    pub c308: ZN,
    pub c309: ZN,
    pub c253: ZN,
    pub c310: ZN,
    pub c311: ZN,
    pub c255: ZN,
    pub c256: ZN,
    pub c312: ZB,
    pub c258: ZN,
    pub c265: ZB,
    pub c266: ZB,
    pub c320: ZN,
    pub c321: ZN,
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (233, "objects[0].collideable"),
    (259, "objects[0].flip.x"),
    (260, "objects[0].flip.y"),
    (261, "objects[0].hitbox.h"),
    (262, "objects[0].hitbox.w"),
    (263, "objects[0].hitbox.x"),
    (264, "objects[0].hitbox.y"),
    (265, "objects[0].rem.x"),
    (266, "objects[0].rem.y"),
    (242, "objects[0].solids"),
    (267, "objects[0].spd.x"),
    (268, "objects[0].spd.y"),
    (244, "objects[0].spr"),
    (246, "objects[0].x"),
    (247, "objects[0].y"),
    (43, "pause_player"),
    (157, "player_spawn.tile"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (14, 234), (15, 235), (19, 236), (18, 237), (22, 238), (23, 239), (24, 240), (4, 241), (12, 242), (3, 243), (13, 244), (0, 245), (1, 246), (2, 247)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 259), (2, 260)]),
    SCell::Obj(&[(17, 261), (16, 262), (1, 263), (2, 264)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 265), (2, 266)]),
    SCell::Obj(&[(1, 267), (2, 268)]),
    SCell::Val,
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
    (151, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (156, 209),
    (158, 210),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 211),
    (173, 212),
    (174, 213),
    (176, 214),
    (177, 215),
    (179, 216),
    (181, 217),
    (183, 218),
    (185, 219),
    (187, 220),
    (190, 221),
    (193, 222),
    (195, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (203, 229),
    (204, 230),
    (231, 248),
    (232, 249),
    (234, 250),
    (235, 251),
    (236, 252),
    (237, 253),
    (238, 254),
    (239, 255),
    (240, 256),
    (241, 257),
    (243, 258),
    (245, 123),
];

/// Outcome 1's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared1 {
    pub c87: ZN,
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

// ---------------- outcome 2 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_2: &[(u32, &str)] = &[
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (233, "objects[0].collideable"),
    (234, "objects[0].delay"),
    (263, "objects[0].flip.x"),
    (264, "objects[0].flip.y"),
    (265, "objects[0].hitbox.h"),
    (266, "objects[0].hitbox.w"),
    (267, "objects[0].hitbox.x"),
    (268, "objects[0].hitbox.y"),
    (269, "objects[0].rem.x"),
    (270, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (271, "objects[0].spd.x"),
    (272, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (246, "objects[0].state"),
    (273, "objects[0].target.x"),
    (274, "objects[0].target.y"),
    (157, "objects[0].type.tile"),
    (249, "objects[0].x"),
    (250, "objects[0].y"),
    (43, "pause_player"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 231), (20, 232), (11, 233), (35, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (25, 246), (34, 247), (0, 248), (1, 249), (2, 250)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 263), (2, 264)]),
    SCell::Obj(&[(17, 265), (16, 266), (1, 267), (2, 268)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 269), (2, 270)]),
    SCell::Obj(&[(1, 271), (2, 272)]),
    SCell::Obj(&[(1, 273), (2, 274)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (153, 206),
    (154, 207),
    (155, 208),
    (156, 209),
    (158, 210),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 211),
    (173, 212),
    (174, 213),
    (176, 214),
    (177, 215),
    (179, 216),
    (181, 217),
    (183, 218),
    (185, 219),
    (187, 220),
    (190, 221),
    (193, 222),
    (195, 223),
    (197, 224),
    (198, 225),
    (199, 226),
    (200, 227),
    (201, 228),
    (203, 229),
    (204, 230),
    (231, 251),
    (232, 252),
    (235, 253),
    (236, 254),
    (237, 255),
    (238, 256),
    (239, 257),
    (240, 258),
    (241, 259),
    (242, 260),
    (244, 261),
    (247, 262),
    (248, 94),
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
    pub c85: ZN,
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
    (175, "balloon.tile"),
    (202, "big_chest.tile"),
    (194, "chest.if_not_fruit"),
    (196, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (188, "fake_wall.if_not_fruit"),
    (189, "fake_wall.tile"),
    (178, "fall_floor.tile"),
    (184, "fly_fruit.if_not_fruit"),
    (186, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (180, "fruit.if_not_fruit"),
    (182, "fruit.tile"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (191, "key.if_not_fruit"),
    (192, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (234, "objects[0].collideable"),
    (292, "objects[0].flip.x"),
    (293, "objects[0].flip.y"),
    (294, "objects[0].hitbox.h"),
    (295, "objects[0].hitbox.w"),
    (296, "objects[0].hitbox.x"),
    (297, "objects[0].hitbox.y"),
    (298, "objects[0].rem.x"),
    (299, "objects[0].rem.y"),
    (243, "objects[0].solids"),
    (300, "objects[0].spd.x"),
    (301, "objects[0].spd.y"),
    (245, "objects[0].spr"),
    (247, "objects[0].x"),
    (248, "objects[0].y"),
    (251, "objects[1].collideable"),
    (252, "objects[1].delay"),
    (302, "objects[1].flip.x"),
    (303, "objects[1].flip.y"),
    (304, "objects[1].hitbox.h"),
    (305, "objects[1].hitbox.w"),
    (306, "objects[1].hitbox.x"),
    (307, "objects[1].hitbox.y"),
    (308, "objects[1].rem.x"),
    (309, "objects[1].rem.y"),
    (261, "objects[1].solids"),
    (310, "objects[1].spd.x"),
    (311, "objects[1].spd.y"),
    (263, "objects[1].spr"),
    (264, "objects[1].state"),
    (312, "objects[1].target.x"),
    (313, "objects[1].target.y"),
    (157, "objects[1].type.tile"),
    (267, "objects[1].x"),
    (268, "objects[1].y"),
    (43, "pause_player"),
    (159, "room.x"),
    (160, "room.y"),
    (85, "seconds"),
    (172, "spring.tile"),
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
    SCell::Arr(&[151, 152]),
    SCell::Obj(&[(7, 153), (5, 154), (6, 155)]),
    SCell::Obj(&[(5, 156), (8, 157), (6, 158)]),
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
    SCell::Obj(&[(1, 159), (2, 160)]),
    SCell::Arr(&[161, 162, 163, 164, 165, 166, 167, 168, 169, 170]),
    SCell::Unk,
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 171), (8, 172), (6, 173)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 174), (8, 175), (6, 176)]),
    SCell::Obj(&[(5, 177), (8, 178), (6, 179)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 180), (5, 181), (8, 182), (6, 183)]),
    SCell::Obj(&[(9, 184), (5, 185), (8, 186), (6, 187)]),
    SCell::Obj(&[(9, 188), (8, 189), (6, 190)]),
    SCell::Obj(&[(9, 191), (8, 192), (6, 193)]),
    SCell::Obj(&[(9, 194), (5, 195), (8, 196), (6, 197)]),
    SCell::Obj(&[(5, 198), (6, 199)]),
    SCell::Obj(&[(7, 200), (5, 201), (8, 202)]),
    SCell::Obj(&[(7, 203), (5, 204)]),
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
    SCell::Obj(&[(21, 232), (20, 233), (11, 234), (14, 235), (15, 236), (19, 237), (18, 238), (22, 239), (23, 240), (24, 241), (4, 242), (12, 243), (3, 244), (13, 245), (0, 246), (1, 247), (2, 248)]),
    SCell::Obj(&[(21, 249), (20, 250), (11, 251), (35, 252), (14, 253), (15, 254), (19, 255), (18, 256), (22, 257), (23, 258), (24, 259), (4, 260), (12, 261), (3, 262), (13, 263), (25, 264), (34, 265), (0, 266), (1, 267), (2, 268)]),
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
    SCell::Clo(26, &[205]),
    SCell::Clo(25, &[205]),
    SCell::Obj(&[(1, 292), (2, 293)]),
    SCell::Obj(&[(17, 294), (16, 295), (1, 296), (2, 297)]),
    SCell::Clo(24, &[205]),
    SCell::Clo(23, &[205]),
    SCell::Clo(27, &[205]),
    SCell::Clo(28, &[205]),
    SCell::Clo(29, &[205]),
    SCell::Obj(&[(1, 298), (2, 299)]),
    SCell::Obj(&[(1, 300), (2, 301)]),
    SCell::Clo(26, &[206]),
    SCell::Clo(25, &[206]),
    SCell::Obj(&[(1, 302), (2, 303)]),
    SCell::Obj(&[(17, 304), (16, 305), (1, 306), (2, 307)]),
    SCell::Clo(24, &[206]),
    SCell::Clo(23, &[206]),
    SCell::Clo(27, &[206]),
    SCell::Clo(28, &[206]),
    SCell::Clo(29, &[206]),
    SCell::Obj(&[(1, 308), (2, 309)]),
    SCell::Obj(&[(1, 310), (2, 311)]),
    SCell::Obj(&[(1, 312), (2, 313)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 205),
    (152, 206),
    (153, 207),
    (154, 208),
    (155, 209),
    (156, 210),
    (158, 211),
    (161, 94),
    (162, 116),
    (163, 118),
    (164, 119),
    (165, 121),
    (166, 122),
    (167, 123),
    (168, 124),
    (169, 125),
    (170, 127),
    (171, 212),
    (173, 213),
    (174, 214),
    (176, 215),
    (177, 216),
    (179, 217),
    (181, 218),
    (183, 219),
    (185, 220),
    (187, 221),
    (190, 222),
    (193, 223),
    (195, 224),
    (197, 225),
    (198, 226),
    (199, 227),
    (200, 228),
    (201, 229),
    (203, 230),
    (204, 231),
    (232, 269),
    (233, 270),
    (235, 271),
    (236, 272),
    (237, 273),
    (238, 274),
    (239, 275),
    (240, 276),
    (241, 277),
    (242, 278),
    (244, 279),
    (246, 123),
    (249, 280),
    (250, 281),
    (253, 282),
    (254, 283),
    (255, 284),
    (256, 285),
    (257, 286),
    (258, 287),
    (259, 288),
    (260, 289),
    (262, 290),
    (265, 291),
    (266, 94),
];

/// Outcome 3's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared3 {
    pub c87: ZN,
    pub c39: ZN,
    pub c84: ZN,
    pub c20: ZN,
    pub c86: ZN,
    pub c294: ZN,
    pub c295: ZN,
    pub c252: ZN,
    pub c309: ZN,
    pub c311: ZN,
    pub c263: ZN,
    pub c264: ZN,
    pub c268: ZN,
    pub c85: ZN,
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[298] = Col::U(AV::Bool(false));
    b.cols[299] = Col::U(AV::Bool(false));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[308] = Col::N(Vec::new());
    b.cols[309] = Col::N(Vec::new());
    b.cols[253] = Col::N(Vec::new());
    b.cols[310] = Col::N(Vec::new());
    b.cols[311] = Col::N(Vec::new());
    b.cols[255] = Col::N(Vec::new());
    b.cols[256] = Col::N(Vec::new());
    b.cols[312] = Col::V(Vec::new());
    b.cols[313] = Col::U(AV::Bool(false));
    b.cols[258] = Col::N(Vec::new());
    b.cols[314] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[265] = Col::V(Vec::new());
    b.cols[266] = Col::V(Vec::new());
    b.cols[318] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[319] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[320] = Col::N(Vec::new());
    b.cols[321] = Col::N(Vec::new());
    b.cols[272] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[273] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
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
pub const KPART1_0: u64 = 3534259214067227465;
pub const KPART2_0: u64 = 17220010034805357520;

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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[308] { v.push(kv.c308.lane(i)); }
        if let Col::N(v) = &mut acc.cols[309] { v.push(kv.c309.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(kv.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[310] { v.push(kv.c310.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(kv.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[255] { v.push(kv.c255.lane(i)); }
        if let Col::N(v) = &mut acc.cols[256] { v.push(kv.c256.lane(i)); }
        if let Col::V(v) = &mut acc.cols[312] {
            v.push(if kv.c312.known & (1 << i) != 0 {
                AV::Bool(kv.c312.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[258] { v.push(kv.c258.lane(i)); }
        if let Col::V(v) = &mut acc.cols[265] {
            v.push(if kv.c265.known & (1 << i) != 0 {
                AV::Bool(kv.c265.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[266] {
            v.push(if kv.c266.known & (1 << i) != 0 {
                AV::Bool(kv.c266.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[320] { v.push(kv.c320.lane(i)); }
        if let Col::N(v) = &mut acc.cols[321] { v.push(kv.c321.lane(i)); }
        if let Col::N(v) = &mut acc.cols[273] { v.push(sh.c273.lane(i)); }
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[259] = Col::U(AV::Bool(false));
    b.cols[260] = Col::U(AV::Bool(false));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[264] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[242] = Col::U(AV::Bool(true));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[244] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
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
pub const KPART1_1: u64 = 11674768501534033049;
pub const KPART2_1: u64 = 4811692208502302106;

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

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[233] = Col::U(AV::Bool(true));
    b.cols[234] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[263] = Col::U(AV::Bool(false));
    b.cols[264] = Col::U(AV::Bool(false));
    b.cols[265] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[266] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[268] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(false));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[246] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[249] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[250] = Col::U(AV::Num(P8::from_raw(8388608i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::V(Vec::new());
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
pub const KPART1_2: u64 = 7552548929090837808;
pub const KPART2_2: u64 = 8363118189267323016;

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

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[180] = Col::U(AV::Bool(true));
    b.cols[182] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[191] = Col::U(AV::Bool(true));
    b.cols[192] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[234] = Col::U(AV::Bool(true));
    b.cols[292] = Col::U(AV::Bool(false));
    b.cols[293] = Col::U(AV::Bool(false));
    b.cols[294] = Col::N(Vec::new());
    b.cols[295] = Col::N(Vec::new());
    b.cols[296] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[297] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Bool(true));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[245] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[247] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[248] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[252] = Col::N(Vec::new());
    b.cols[302] = Col::U(AV::Bool(false));
    b.cols[303] = Col::U(AV::Bool(false));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[307] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[308] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::N(Vec::new());
    b.cols[261] = Col::U(AV::Bool(false));
    b.cols[310] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[311] = Col::N(Vec::new());
    b.cols[263] = Col::N(Vec::new());
    b.cols[264] = Col::N(Vec::new());
    b.cols[312] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[267] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[268] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
    for (cell, _) in OUT_UBOOL_3 {
        b.cols[*cell as usize] = Col::U(AV::UBool);
    }
    b
}

/// The SOUND (full boundary) row key's per-outcome CONSTANT
/// prefix: shape hash + the uniform cells' `cell_mix` sum. The
/// per-lane cells are summed by the graph into `kv.h1/h2`, and
/// `append` closes the key with `mix64(KPART + kv.h)`, which is
/// byte-identical to `Rt2::boundary`'s own row key.
pub const KPART1_3: u64 = 13761463011427019069;
pub const KPART2_3: u64 = 15879343760781235880;

/// Append this assignment's lanes that TAKE outcome 3 and
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
pub fn append3(
    acc: &mut Rt2, sh: &KShared3, kv: &KOut3, take: u16,
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
        let k0 = mix64(KPART1_3.wrapping_add(h1[i]));
        let k1 = mix64(KPART2_3.wrapping_add(h2[i]));
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
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[294] { v.push(sh.c294.lane(i)); }
        if let Col::N(v) = &mut acc.cols[295] { v.push(sh.c295.lane(i)); }
        if let Col::N(v) = &mut acc.cols[252] { v.push(sh.c252.lane(i)); }
        if let Col::N(v) = &mut acc.cols[309] { v.push(sh.c309.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(sh.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[263] { v.push(sh.c263.lane(i)); }
        if let Col::N(v) = &mut acc.cols[264] { v.push(sh.c264.lane(i)); }
        if let Col::N(v) = &mut acc.cols[268] { v.push(sh.c268.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
        if !org.is_empty() { acc.origin.push(org[i]); }
        wrote |= 1 << i;
        acc.width += 1;
        if celeste_engine::runtime2::key_check() { acc.row_keys.push(key); }
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
    fn o3(&mut self, _mask: u8, take: u16, sh: &KShared3, v: &KOut3) {
        append3(&mut self.accs[3], sh, v, take, self.n, &mut self.seen[3], self.org, self.skip);
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
    let r_c86: ZN = rin.c86;
    let r_c87: ZN = rin.c87;
    let r_c88: ZN = rin.c88;
    let r_c234: ZB = ZB { val: rin.c234, known: ALL };
    let r_c243: ZB = ZB { val: rin.c243, known: ALL };
    let r_c245: ZN = rin.c245;
    let r_c247: ZN = rin.c247;
    let r_c248: ZN = rin.c248;
    let r_c251: ZB = ZB { val: rin.c251, known: ALL };
    let r_c252: ZN = rin.c252;
    let r_c261: ZB = ZB { val: rin.c261, known: ALL };
    let r_c263: ZN = rin.c263;
    let r_c264: ZN = rin.c264;
    let r_c267: ZN = rin.c267;
    let r_c268: ZN = rin.c268;
    let r_c292: ZB = ZB { val: rin.c292, known: ALL };
    let r_c293: ZB = ZB { val: rin.c293, known: ALL };
    let r_c298: ZN = rin.c298;
    let r_c299: ZN = rin.c299;
    let r_c300: ZN = rin.c300;
    let r_c301: ZN = rin.c301;
    let r_c302: ZB = ZB { val: rin.c302, known: ALL };
    let r_c303: ZB = ZB { val: rin.c303, known: ALL };
    let r_c308: ZN = rin.c308;
    let r_c309: ZN = rin.c309;
    let r_c310: ZN = rin.c310;
    let r_c311: ZN = rin.c311;
    let r_c312: ZN = rin.c312;
    let r_c313: ZN = rin.c313;
    let n69: ZB = zb_not(r_c41);
    let n70: ZB = zb_not(r_c42);
    let n71: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n72: ZB = zb_not(r_c292);
    let n73: ZB = zb_not(r_c293);
    let n74: bool = P8::from_raw(0i32) == u.c296;
    let n75: bool = P8::from_raw(0i32) == u.c297;
    let n76: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c298);
    let n77: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c299);
    let n78: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c300);
    let n79: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c301);
    let n80: ZB = zn_eq(zn_splat(P8::from_raw(4194304i32)), r_c245);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c247);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(2097152i32)), r_c248);
    let n83: ZB = zb_not(r_c302);
    let n84: ZB = zb_not(r_c303);
    let n85: bool = P8::from_raw(524288i32) == u.c304;
    let n86: bool = P8::from_raw(524288i32) == u.c305;
    let n87: bool = P8::from_raw(0i32) == u.c306;
    let n88: bool = P8::from_raw(0i32) == u.c307;
    let n89: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c308);
    let n90: ZB = zb_not(r_c261);
    let n91: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c310);
    let n92: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c264);
    let n93: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c312);
    let n94: ZB = zn_eq(zn_splat(P8::from_raw(6291456i32)), r_c313);
    let n95: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c267);
    let n96: ZB = zb_not(r_c43);
    let n97: ZB = zb_not(r_c38);
    let n101: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n102: ZN = zn_rem(n101, zn_splat(P8::from_raw(1966080i32)));
    let n103: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n102);
    let n105: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n106: ZN = zn_rem(n105, zn_splat(P8::from_raw(3932160i32)));
    let n107: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n106);
    let n108: ZN = zsel_n(n103, n106, r_c85);
    let n109: ZB = zb_not(n92);
    let n110: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c264);
    let n111: ZB = zb_not(n110);
    let n112: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c264);
    let n113: ZN = zn_sub(r_c252, zn_splat(P8::from_raw(65536i32)));
    let n114: ZB = zn_lt(n113, zn_splat(P8::from_raw(0i32)));
    let n126: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n127: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n128: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n152: ZN = zsel_n(n107, n126, r_c86);
    let n153: ZN = zsel_n(n103, n152, r_c86);
    let n154: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c311);
    let n155: ZB = zb_not(n154);
    let n156: ZB = zb_and(n109, n127);
    let n157: ZB = zb_and(n111, n156);
    let n158: ZB = zb_and(n112, n157);
    let n159: ZB = zb_and(n114, n158);
    let n167: ZN = zn_add(r_c309, r_c311);
    let n168: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n167);
    let n169: ZN = zn_flr(n168);
    let n170: ZN = zn_add(r_c268, n169);
    let n171: ZN = zsel_n(n155, n170, r_c268);
    let n172: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n171);
    let n173: ZN = zn_div(n172, zn_splat(P8::from_raw(524288i32)));
    let n174: ZN = zn_flr(n173);
    let n175: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n174);
    let n176: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n172);
    let n177: ZN = zn_sub(n176, zn_splat(P8::from_raw(65536i32)));
    let n178: ZN = zn_div(n177, zn_splat(P8::from_raw(524288i32)));
    let n179: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n178);
    let n180: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n175);
    let n181: ZB = zn_le(n180, n179);
    let n182: ZB = zn_gt(n180, n179);
    let n183: ZB = zb_and(n159, n182);
    let n184: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n180);
    let n185: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n184);
    let n186: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n185);
    let n187: ZN = zn_rem(n177, zn_splat(P8::from_raw(524288i32)));
    let n188: ZB = zn_ge(n187, zn_splat(P8::from_raw(393216i32)));
    let n189: ZN = zn_mul(n180, zn_splat(P8::from_raw(524288i32)));
    let n190: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n189);
    let n191: ZB = zn_eq(n176, n190);
    let n192: ZB = zb_and(n109, n111);
    let n193: ZB = zb_and(n112, n192);
    let n194: ZB = zb_and(n114, n193);
    let n195: ZB = zb_and(n127, n194);
    let n196: ZB = zb_and(n181, n195);
    let n197: ZB = zb_or(n188, n191);
    let n198: ZB = zb_and(n186, n197);
    let n199: ZB = zb_not(n198);
    let n200: ZB = zb_and(n196, n198);
    let n201: ZB = zb_and(n196, n199);
    let n202: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n185);
    let n203: ZN = zn_rem(n172, zn_splat(P8::from_raw(524288i32)));
    let n204: ZB = zn_le(n203, zn_splat(P8::from_raw(131072i32)));
    let n205: ZB = zb_and(n202, n204);
    let n206: ZB = zb_not(n205);
    let n207: ZB = zb_and(n201, n205);
    let n208: ZB = zb_and(n201, n206);
    let n209: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n185);
    let n210: ZB = zb_not(n209);
    let n211: ZB = zb_and(n208, n209);
    let n212: ZB = zb_and(n208, n210);
    let n213: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n185);
    let n214: ZB = zb_not(n213);
    let n215: ZB = zb_and(n212, n213);
    let n216: ZB = zb_and(n212, n214);
    let n217: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n175);
    let n218: ZB = zn_le(n217, n179);
    let n219: ZB = zn_gt(n217, n179);
    let n220: ZB = zb_and(n216, n218);
    let n221: ZB = zb_and(n216, n219);
    let n222: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n217);
    let n223: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n222);
    let n224: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n223);
    let n225: ZN = zn_mul(n217, zn_splat(P8::from_raw(524288i32)));
    let n226: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n225);
    let n227: ZB = zn_eq(n176, n226);
    let n228: ZB = zb_or(n188, n227);
    let n229: ZB = zb_and(n224, n228);
    let n230: ZB = zb_not(n229);
    let n231: ZB = zb_and(n220, n229);
    let n232: ZB = zb_and(n220, n230);
    let n233: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n223);
    let n234: ZB = zb_and(n204, n233);
    let n235: ZB = zb_not(n234);
    let n236: ZB = zb_and(n232, n234);
    let n237: ZB = zb_and(n232, n235);
    let n238: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n223);
    let n239: ZB = zb_not(n238);
    let n240: ZB = zb_and(n237, n238);
    let n241: ZB = zb_and(n237, n239);
    let n242: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n223);
    let n243: ZB = zb_not(n242);
    let n244: ZB = zb_and(n241, n242);
    let n245: ZB = zb_and(n241, n243);
    let n246: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n175);
    let n247: ZB = zn_le(n246, n179);
    let n248: ZB = zn_gt(n246, n179);
    let n249: ZB = zb_and(n245, n247);
    let n250: ZB = zb_and(n245, n248);
    let n251: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n246);
    let n252: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n251);
    let n253: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n252);
    let n254: ZN = zn_mul(n246, zn_splat(P8::from_raw(524288i32)));
    let n255: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n254);
    let n256: ZB = zn_eq(n176, n255);
    let n257: ZB = zb_or(n188, n256);
    let n258: ZB = zb_and(n253, n257);
    let n259: ZB = zb_not(n258);
    let n260: ZB = zb_and(n249, n258);
    let n261: ZB = zb_and(n249, n259);
    let n262: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n252);
    let n263: ZB = zb_and(n204, n262);
    let n264: ZB = zb_not(n263);
    let n265: ZB = zb_and(n261, n263);
    let n266: ZB = zb_and(n261, n264);
    let n267: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n252);
    let n268: ZB = zb_not(n267);
    let n269: ZB = zb_and(n266, n267);
    let n270: ZB = zb_and(n266, n268);
    let n271: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n252);
    let n272: ZB = zb_not(n271);
    let n273: ZB = zb_and(n270, n271);
    let n274: ZB = zb_and(n270, n272);
    let n275: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n175);
    let n276: ZB = zn_gt(n275, n179);
    let n277: ZB = zb_or(n250, n274);
    let n278: ZB = zb_or(n248, n276);
    let n279: ZB = zb_or(n221, n277);
    let n280: ZB = zb_or(n219, n278);
    let n281: ZB = zb_or(n183, n279);
    let n282: ZB = zb_or(n182, n280);
    let n283: ZB = zn_le(n171, zn_splat(P8::from_raw(8388608i32)));
    let n284: ZB = zb_and(n281, n283);
    let n285: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n172);
    let n286: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n285, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n287: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n285);
    let n288: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n176);
    let n289: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n288);
    let n290: ZB = zb_and(n287, n289);
    let n291: ZB = zb_or(n286, n290);
    let n292: ZB = zb_not(n291);
    let n293: ZN = zsel_n(n291, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n294: ZN = zsel_n(n292, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n295: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n294);
    let n296: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n294);
    let n297: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n172);
    let n298: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n297);
    let n299: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n176);
    let n300: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n299);
    let n301: ZB = zb_and(n298, n300);
    let n302: ZN = zsel_n(n292, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n303: ZB = zn_gt(n293, zn_splat(P8::from_raw(0i32)));
    let n304: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n297, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n305: ZB = zb_or(n301, n304);
    let n306: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n297, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n307: ZB = zb_or(n301, n306);
    let n308: ZN = zsel_n(n307, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n309: ZN = zsel_n(n305, zn_splat(P8::from_raw(-65536i32)), n308);
    let n310: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n309);
    let n311: ZB = zb_not(n310);
    let n312: ZN = zn_neg(n309);
    let n313: ZN = zn_mul(n312, zn_splat(P8::from_raw(131072i32)));
    let n314: ZN = zsel_n(n311, n313, zn_splat(P8::from_raw(0i32)));
    let n315: ZN = zsel_n(n311, zn_splat(P8::from_raw(-131072i32)), n302);
    let n316: ZN = zsel_n(n303, zn_splat(P8::from_raw(0i32)), n293);
    let n317: ZN = zsel_n(n303, zn_splat(P8::from_raw(0i32)), n314);
    let n318: ZN = zsel_n(n303, zn_splat(P8::from_raw(-131072i32)), n315);
    let n319: ZB = zn_lt(n171, zn_splat(P8::from_raw(-262144i32)));
    let n320: ZB = zn_ge(n171, zn_splat(P8::from_raw(-262144i32)));
    let n321: ZB = zb_and(n284, n319);
    let n326: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n295);
    let n327: ZN = zsel_n(n311, n313, n326);
    let n328: ZN = zsel_n(n303, n326, n327);
    let n329: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n296);
    let n330: ZN = zsel_n(n311, n313, n329);
    let n331: ZN = zsel_n(n303, n329, n330);
    let n333: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n334: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n333);
    let n335: ZB = zb_or(n211, n215);
    let n336: ZB = zb_or(n207, n335);
    let n337: ZB = zb_or(n200, n336);
    let n338: ZB = zb_or(n240, n244);
    let n339: ZB = zb_or(n236, n338);
    let n340: ZB = zb_or(n231, n339);
    let n341: ZB = zb_or(n269, n273);
    let n342: ZB = zb_or(n265, n341);
    let n343: ZB = zb_or(n260, n342);
    let n344: ZB = zb_or(n340, n343);
    let n345: ZB = zb_or(n337, n344);
    let n346: ZB = zn_gt(n171, zn_splat(P8::from_raw(8388608i32)));
    let n347: ZN = zsel_n(n346, n334, n333);
    let n348: ZB = zb_and(n281, n346);
    let n349: ZN = zsel_n(n345, n347, n333);
    let n350: ZB = zb_or(n345, n348);
    let n351: ZB = zb_or(n282, n345);
    let n352: ZB = zb_and(n319, n350);
    let n356: ZN = zsel_n(n321, r_c87, n349);
    let n357: ZN = zsel_n(n321, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n358: ZB = zb_not(n321);
    let n359: ZB = zb_or(n321, n352);
    let n360: ZB = zsel_b(n321, n282, n351);
    let n365: ZB = zb_not(n112);
    let n366: ZB = zn_ge(n113, zn_splat(P8::from_raw(0i32)));
    let n367: ZN = zsel_n(n112, n113, r_c252);
    let n368: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n369: ZN = zsel_n(n128, n368, r_c20);
    let n370: ZB = zb_and(n92, n127);
    let n371: ZN = zn_add(r_c311, zn_splat(P8::from_raw(32768i32)));
    let n372: ZB = zn_gt(n371, zn_splat(P8::from_raw(0i32)));
    let n373: ZB = zn_gt(r_c252, zn_splat(P8::from_raw(0i32)));
    let n374: ZB = zb_and(n109, n110);
    let n375: ZB = zb_and(n127, n374);
    let n376: ZB = zb_and(n372, n373);
    let n377: ZN = zsel_n(n376, n113, r_c252);
    let n378: ZN = zsel_n(n376, zn_splat(P8::from_raw(0i32)), n371);
    let n379: ZB = zn_gt(n378, zn_splat(P8::from_raw(0i32)));
    let n380: ZB = zb_and(n157, n365);
    let n381: ZB = zb_and(n158, n366);
    let n382: ZN = zsel_n(n112, zn_splat(P8::from_raw(393216i32)), r_c263);
    let n383: ZB = zb_or(n380, n381);
    let n384: ZN = zsel_n(n110, r_c263, n382);
    let n385: ZN = zsel_n(n92, r_c263, n384);
    let n386: ZN = zsel_n(n128, r_c263, n385);
    let n387: ZN = zsel_n(n128, zn_splat(u.c294), zn_splat(P8::from_raw(1048576i32)));
    let n388: ZN = zsel_n(n128, zn_splat(u.c295), zn_splat(P8::from_raw(1048576i32)));
    let n389: ZN = zn_sub(n168, zn_splat(P8::from_raw(32768i32)));
    let n390: ZN = zn_sub(n389, n169);
    let n391: ZN = zsel_n(n155, n390, r_c309);
    let n392: ZB = zn_lt(n171, zn_splat(P8::from_raw(7340032i32)));
    let n393: ZN = zsel_n(n392, zn_splat(P8::from_raw(196608i32)), r_c252);
    let n394: ZN = zsel_n(n392, zn_splat(P8::from_raw(65536i32)), r_c264);
    let n395: ZB = zn_gt(n171, zn_splat(P8::from_raw(6291456i32)));
    let n396: ZB = zb_and(n379, n395);
    let n397: ZN = zsel_n(n396, zn_splat(P8::from_raw(327680i32)), n377);
    let n398: ZN = zsel_n(n396, zn_splat(P8::from_raw(131072i32)), r_c264);
    let n399: ZN = zsel_n(n396, zn_splat(P8::from_raw(6291456i32)), n171);
    let n400: ZN = zsel_n(n396, zn_splat(P8::from_raw(0i32)), n378);
    let n401: ZN = zsel_n(n110, n397, n367);
    let n402: ZN = zsel_n(n110, n398, r_c264);
    let n403: ZN = zsel_n(n110, n399, n171);
    let n404: ZN = zsel_n(n110, n400, r_c311);
    let n405: ZB = zb_or(n375, n383);
    let n406: ZN = zsel_n(n92, n393, n401);
    let n407: ZN = zsel_n(n92, n394, n402);
    let n408: ZN = zsel_n(n92, n171, n403);
    let n409: ZN = zsel_n(n92, r_c311, n404);
    let n410: ZB = zb_or(n370, n405);
    let n411: ZN = zsel_n(n128, r_c252, n406);
    let n412: ZN = zsel_n(n128, r_c264, n407);
    let n413: ZN = zsel_n(n128, r_c268, n408);
    let n414: ZN = zsel_n(n128, r_c309, n391);
    let n415: ZN = zsel_n(n128, r_c311, n409);
    let n416: ZB = zb_or(n128, n410);
    let n418: ZW = zw_cellmix_n(39u64, r_c39, 1542469173u64);
    let n419: ZW = zw_cellmix_n(39u64, r_c39, 668265263u64);
    let n420: ZW = zw_add(zw_splat(0u64), n418);
    let n421: ZW = zw_add(zw_splat(0u64), n419);
    let n422: ZW = zw_cellmix_n(84u64, n102, 1542469173u64);
    let n423: ZW = zw_cellmix_n(84u64, n102, 668265263u64);
    let n424: ZW = zw_add(n420, n422);
    let n425: ZW = zw_add(n421, n423);
    let n426: ZW = zw_cellmix_n(85u64, n108, 1542469173u64);
    let n427: ZW = zw_cellmix_n(85u64, n108, 668265263u64);
    let n428: ZW = zw_add(n424, n426);
    let n429: ZW = zw_add(n425, n427);
    let n430: ZW = zw_cellmix_n(86u64, n153, 1542469173u64);
    let n431: ZW = zw_cellmix_n(86u64, n153, 668265263u64);
    let n432: ZW = zw_add(n428, n430);
    let n433: ZW = zw_add(n429, n431);
    let n434: ZW = zw_cellmix_n(87u64, r_c87, 1542469173u64);
    let n435: ZW = zw_cellmix_n(87u64, r_c87, 668265263u64);
    let n436: ZW = zw_add(n432, n434);
    let n437: ZW = zw_add(n433, n435);
    let n438: ZW = zw_cellmix_n(273u64, n171, 1542469173u64);
    let n439: ZW = zw_cellmix_n(273u64, n171, 668265263u64);
    let n440: ZW = zw_add(n436, n438);
    let n441: ZW = zw_add(n437, n439);
    let n442: ZW = zw_cellmix_n(20u64, r_c20, 1542469173u64);
    let n443: ZW = zw_cellmix_n(20u64, r_c20, 668265263u64);
    let n444: ZW = zw_add(n440, n442);
    let n445: ZW = zw_add(n441, n443);
    let n446: ZW = zw_cellmix_b(41u64, zb_splat(false), 1542469173u64);
    let n447: ZW = zw_cellmix_b(41u64, zb_splat(false), 668265263u64);
    let n448: ZW = zw_add(n444, n446);
    let n449: ZW = zw_add(n445, n447);
    let n450: ZW = zw_cellmix_n(253u64, zn_splat(P8::from_raw(-65536i32)), 1542469173u64);
    let n451: ZW = zw_cellmix_n(253u64, zn_splat(P8::from_raw(-65536i32)), 668265263u64);
    let n452: ZW = zw_add(n448, n450);
    let n453: ZW = zw_add(n449, n451);
    let n454: ZW = zw_cellmix_n(255u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n455: ZW = zw_cellmix_n(255u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n456: ZW = zw_add(n452, n454);
    let n457: ZW = zw_add(n453, n455);
    let n458: ZW = zw_cellmix_n(256u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n459: ZW = zw_cellmix_n(256u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n460: ZW = zw_add(n456, n458);
    let n461: ZW = zw_add(n457, n459);
    let n462: ZW = zw_cellmix_n(258u64, n293, 1542469173u64);
    let n463: ZW = zw_cellmix_n(258u64, n293, 668265263u64);
    let n464: ZW = zw_add(n460, n462);
    let n465: ZW = zw_add(n461, n463);
    let n466: ZW = zw_cellmix_b(265u64, zb_splat(false), 1542469173u64);
    let n467: ZW = zw_cellmix_b(265u64, zb_splat(false), 668265263u64);
    let n468: ZW = zw_add(n464, n466);
    let n469: ZW = zw_add(n465, n467);
    let n470: ZW = zw_cellmix_b(266u64, zb_splat(false), 1542469173u64);
    let n471: ZW = zw_cellmix_b(266u64, zb_splat(false), 668265263u64);
    let n472: ZW = zw_add(n468, n470);
    let n473: ZW = zw_add(n469, n471);
    let n474: ZW = zw_cellmix_n(308u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n475: ZW = zw_cellmix_n(308u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n476: ZW = zw_add(n472, n474);
    let n477: ZW = zw_add(n473, n475);
    let n478: ZW = zw_cellmix_n(309u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n479: ZW = zw_cellmix_n(309u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n480: ZW = zw_add(n476, n478);
    let n481: ZW = zw_add(n477, n479);
    let n482: ZW = zw_cellmix_n(310u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n483: ZW = zw_cellmix_n(310u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n484: ZW = zw_add(n480, n482);
    let n485: ZW = zw_add(n481, n483);
    let n486: ZW = zw_cellmix_n(311u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n487: ZW = zw_cellmix_n(311u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n488: ZW = zw_add(n484, n486);
    let n489: ZW = zw_add(n485, n487);
    let n490: ZW = zw_cellmix_b(312u64, zb_splat(false), 1542469173u64);
    let n491: ZW = zw_cellmix_b(312u64, zb_splat(false), 668265263u64);
    let n492: ZW = zw_add(n488, n490);
    let n493: ZW = zw_add(n489, n491);
    let n494: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n495: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n496: ZW = zw_add(n492, n494);
    let n497: ZW = zw_add(n493, n495);
    let n498: ZW = zw_cellmix_n(321u64, n302, 1542469173u64);
    let n499: ZW = zw_cellmix_n(321u64, n302, 668265263u64);
    let n500: ZW = zw_add(n496, n498);
    let n501: ZW = zw_add(n497, n499);
    let n502: ZW = zw_cellmix_b(312u64, zb_splat(true), 1542469173u64);
    let n503: ZW = zw_cellmix_b(312u64, zb_splat(true), 668265263u64);
    let n504: ZW = zw_add(n488, n502);
    let n505: ZW = zw_add(n489, n503);
    let n506: ZW = zw_cellmix_n(320u64, n326, 1542469173u64);
    let n507: ZW = zw_cellmix_n(320u64, n326, 668265263u64);
    let n508: ZW = zw_add(n504, n506);
    let n509: ZW = zw_add(n505, n507);
    let n510: ZW = zw_add(n508, n498);
    let n511: ZW = zw_add(n509, n499);
    let n512: ZW = zw_cellmix_n(320u64, n329, 1542469173u64);
    let n513: ZW = zw_cellmix_n(320u64, n329, 668265263u64);
    let n514: ZW = zw_add(n492, n512);
    let n515: ZW = zw_add(n493, n513);
    let n516: ZW = zw_add(n514, n498);
    let n517: ZW = zw_add(n515, n499);
    let n518: ZW = zw_cellmix_n(258u64, n316, 1542469173u64);
    let n519: ZW = zw_cellmix_n(258u64, n316, 668265263u64);
    let n520: ZW = zw_add(n460, n518);
    let n521: ZW = zw_add(n461, n519);
    let n522: ZW = zw_add(n520, n466);
    let n523: ZW = zw_add(n521, n467);
    let n524: ZW = zw_cellmix_b(266u64, zb_splat(true), 1542469173u64);
    let n525: ZW = zw_cellmix_b(266u64, zb_splat(true), 668265263u64);
    let n526: ZW = zw_add(n522, n524);
    let n527: ZW = zw_add(n523, n525);
    let n528: ZW = zw_add(n526, n474);
    let n529: ZW = zw_add(n527, n475);
    let n530: ZW = zw_add(n528, n478);
    let n531: ZW = zw_add(n529, n479);
    let n532: ZW = zw_add(n530, n482);
    let n533: ZW = zw_add(n531, n483);
    let n534: ZW = zw_add(n532, n486);
    let n535: ZW = zw_add(n533, n487);
    let n536: ZW = zw_add(n534, n490);
    let n537: ZW = zw_add(n535, n491);
    let n538: ZW = zw_cellmix_n(320u64, n317, 1542469173u64);
    let n539: ZW = zw_cellmix_n(320u64, n317, 668265263u64);
    let n540: ZW = zw_add(n536, n538);
    let n541: ZW = zw_add(n537, n539);
    let n542: ZW = zw_cellmix_n(321u64, n318, 1542469173u64);
    let n543: ZW = zw_cellmix_n(321u64, n318, 668265263u64);
    let n544: ZW = zw_add(n540, n542);
    let n545: ZW = zw_add(n541, n543);
    let n546: ZW = zw_add(n534, n502);
    let n547: ZW = zw_add(n535, n503);
    let n548: ZW = zw_cellmix_n(320u64, n328, 1542469173u64);
    let n549: ZW = zw_cellmix_n(320u64, n328, 668265263u64);
    let n550: ZW = zw_add(n546, n548);
    let n551: ZW = zw_add(n547, n549);
    let n552: ZW = zw_add(n550, n542);
    let n553: ZW = zw_add(n551, n543);
    let n554: ZW = zw_cellmix_n(320u64, n331, 1542469173u64);
    let n555: ZW = zw_cellmix_n(320u64, n331, 668265263u64);
    let n556: ZW = zw_add(n536, n554);
    let n557: ZW = zw_add(n537, n555);
    let n558: ZW = zw_add(n556, n542);
    let n559: ZW = zw_add(n557, n543);
    let n560: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n561: ZW = zw_cellmix_n(20u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n562: ZW = zw_add(n440, n560);
    let n563: ZW = zw_add(n441, n561);
    let n564: ZW = zw_cellmix_b(41u64, zb_splat(true), 1542469173u64);
    let n565: ZW = zw_cellmix_b(41u64, zb_splat(true), 668265263u64);
    let n566: ZW = zw_add(n562, n564);
    let n567: ZW = zw_add(n563, n565);
    let n568: ZW = zw_cellmix_n(253u64, zn_splat(P8::from_raw(655360i32)), 1542469173u64);
    let n569: ZW = zw_cellmix_n(253u64, zn_splat(P8::from_raw(655360i32)), 668265263u64);
    let n570: ZW = zw_add(n566, n568);
    let n571: ZW = zw_add(n567, n569);
    let n572: ZW = zw_cellmix_n(255u64, zn_splat(P8::from_raw(262144i32)), 1542469173u64);
    let n573: ZW = zw_cellmix_n(255u64, zn_splat(P8::from_raw(262144i32)), 668265263u64);
    let n574: ZW = zw_add(n570, n572);
    let n575: ZW = zw_add(n571, n573);
    let n576: ZW = zw_cellmix_n(256u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n577: ZW = zw_cellmix_n(256u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n578: ZW = zw_add(n574, n576);
    let n579: ZW = zw_add(n575, n577);
    let n580: ZW = zw_add(n578, n462);
    let n581: ZW = zw_add(n579, n463);
    let n582: ZW = zw_cellmix_b(265u64, zb_splat(true), 1542469173u64);
    let n583: ZW = zw_cellmix_b(265u64, zb_splat(true), 668265263u64);
    let n584: ZW = zw_add(n580, n582);
    let n585: ZW = zw_add(n581, n583);
    let n586: ZW = zw_add(n584, n470);
    let n587: ZW = zw_add(n585, n471);
    let n588: ZW = zw_cellmix_n(308u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n589: ZW = zw_cellmix_n(308u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n590: ZW = zw_add(n586, n588);
    let n591: ZW = zw_add(n587, n589);
    let n592: ZW = zw_cellmix_n(309u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n593: ZW = zw_cellmix_n(309u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n594: ZW = zw_add(n590, n592);
    let n595: ZW = zw_add(n591, n593);
    let n596: ZW = zw_cellmix_n(310u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n597: ZW = zw_cellmix_n(310u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n598: ZW = zw_add(n594, n596);
    let n599: ZW = zw_add(n595, n597);
    let n600: ZW = zw_add(n598, n486);
    let n601: ZW = zw_add(n599, n487);
    let n602: ZW = zw_add(n600, n490);
    let n603: ZW = zw_add(n601, n491);
    let n604: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(65536i32)), 1542469173u64);
    let n605: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(65536i32)), 668265263u64);
    let n606: ZW = zw_add(n602, n604);
    let n607: ZW = zw_add(n603, n605);
    let n608: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(0i32)), 1542469173u64);
    let n609: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(0i32)), 668265263u64);
    let n610: ZW = zw_add(n606, n608);
    let n611: ZW = zw_add(n607, n609);
    let n612: ZW = zw_cellmix_n(310u64, zn_splat(P8::from_raw(-131072i32)), 1542469173u64);
    let n613: ZW = zw_cellmix_n(310u64, zn_splat(P8::from_raw(-131072i32)), 668265263u64);
    let n614: ZW = zw_add(n594, n612);
    let n615: ZW = zw_add(n595, n613);
    let n616: ZW = zw_add(n614, n486);
    let n617: ZW = zw_add(n615, n487);
    let n618: ZW = zw_add(n616, n502);
    let n619: ZW = zw_add(n617, n503);
    let n620: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n621: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n622: ZW = zw_add(n618, n620);
    let n623: ZW = zw_add(n619, n621);
    let n624: ZW = zw_add(n622, n608);
    let n625: ZW = zw_add(n623, n609);
    let n626: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n627: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n628: ZW = zw_add(n602, n626);
    let n629: ZW = zw_add(n603, n627);
    let n630: ZW = zw_add(n628, n608);
    let n631: ZW = zw_add(n629, n609);
    let n632: ZW = zw_cellmix_n(308u64, zn_splat(P8::from_raw(69510i32)), 1542469173u64);
    let n633: ZW = zw_cellmix_n(308u64, zn_splat(P8::from_raw(69510i32)), 668265263u64);
    let n634: ZW = zw_add(n586, n632);
    let n635: ZW = zw_add(n587, n633);
    let n636: ZW = zw_cellmix_n(309u64, zn_splat(P8::from_raw(98304i32)), 1542469173u64);
    let n637: ZW = zw_cellmix_n(309u64, zn_splat(P8::from_raw(98304i32)), 668265263u64);
    let n638: ZW = zw_add(n634, n636);
    let n639: ZW = zw_add(n635, n637);
    let n640: ZW = zw_add(n638, n482);
    let n641: ZW = zw_add(n639, n483);
    let n642: ZW = zw_cellmix_n(311u64, zn_splat(P8::from_raw(-98304i32)), 1542469173u64);
    let n643: ZW = zw_cellmix_n(311u64, zn_splat(P8::from_raw(-98304i32)), 668265263u64);
    let n644: ZW = zw_add(n640, n642);
    let n645: ZW = zw_add(n641, n643);
    let n646: ZW = zw_add(n644, n490);
    let n647: ZW = zw_add(n645, n491);
    let n648: ZW = zw_add(n646, n494);
    let n649: ZW = zw_add(n647, n495);
    let n650: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(-327680i32)), 1542469173u64);
    let n651: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(-327680i32)), 668265263u64);
    let n652: ZW = zw_add(n648, n650);
    let n653: ZW = zw_add(n649, n651);
    let n654: ZW = zw_add(n634, n592);
    let n655: ZW = zw_add(n635, n593);
    let n656: ZW = zw_add(n654, n612);
    let n657: ZW = zw_add(n655, n613);
    let n658: ZW = zw_add(n656, n642);
    let n659: ZW = zw_add(n657, n643);
    let n660: ZW = zw_add(n658, n502);
    let n661: ZW = zw_add(n659, n503);
    let n662: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n663: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n664: ZW = zw_add(n660, n662);
    let n665: ZW = zw_add(n661, n663);
    let n666: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(-231700i32)), 1542469173u64);
    let n667: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(-231700i32)), 668265263u64);
    let n668: ZW = zw_add(n664, n666);
    let n669: ZW = zw_add(n665, n667);
    let n670: ZW = zw_add(n654, n596);
    let n671: ZW = zw_add(n655, n597);
    let n672: ZW = zw_add(n670, n642);
    let n673: ZW = zw_add(n671, n643);
    let n674: ZW = zw_add(n672, n490);
    let n675: ZW = zw_add(n673, n491);
    let n676: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n677: ZW = zw_cellmix_n(320u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n678: ZW = zw_add(n674, n676);
    let n679: ZW = zw_add(n675, n677);
    let n680: ZW = zw_add(n678, n666);
    let n681: ZW = zw_add(n679, n667);
    let n682: ZW = zw_cellmix_n(311u64, zn_splat(P8::from_raw(131072i32)), 1542469173u64);
    let n683: ZW = zw_cellmix_n(311u64, zn_splat(P8::from_raw(131072i32)), 668265263u64);
    let n684: ZW = zw_add(n640, n682);
    let n685: ZW = zw_add(n641, n683);
    let n686: ZW = zw_add(n684, n490);
    let n687: ZW = zw_add(n685, n491);
    let n688: ZW = zw_add(n686, n494);
    let n689: ZW = zw_add(n687, n495);
    let n690: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(327680i32)), 1542469173u64);
    let n691: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(327680i32)), 668265263u64);
    let n692: ZW = zw_add(n688, n690);
    let n693: ZW = zw_add(n689, n691);
    let n694: ZW = zw_add(n656, n682);
    let n695: ZW = zw_add(n657, n683);
    let n696: ZW = zw_add(n694, n502);
    let n697: ZW = zw_add(n695, n503);
    let n698: ZW = zw_add(n696, n662);
    let n699: ZW = zw_add(n697, n663);
    let n700: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(231700i32)), 1542469173u64);
    let n701: ZW = zw_cellmix_n(321u64, zn_splat(P8::from_raw(231700i32)), 668265263u64);
    let n702: ZW = zw_add(n698, n700);
    let n703: ZW = zw_add(n699, n701);
    let n704: ZW = zw_add(n670, n682);
    let n705: ZW = zw_add(n671, n683);
    let n706: ZW = zw_add(n704, n490);
    let n707: ZW = zw_add(n705, n491);
    let n708: ZW = zw_add(n706, n676);
    let n709: ZW = zw_add(n707, n677);
    let n710: ZW = zw_add(n708, n700);
    let n711: ZW = zw_add(n709, n701);
    let n712: ZW = zw_add(n578, n518);
    let n713: ZW = zw_add(n579, n519);
    let n714: ZW = zw_add(n712, n582);
    let n715: ZW = zw_add(n713, n583);
    let n716: ZW = zw_add(n714, n524);
    let n717: ZW = zw_add(n715, n525);
    let n718: ZW = zw_add(n716, n588);
    let n719: ZW = zw_add(n717, n589);
    let n720: ZW = zw_add(n718, n592);
    let n721: ZW = zw_add(n719, n593);
    let n722: ZW = zw_add(n720, n596);
    let n723: ZW = zw_add(n721, n597);
    let n724: ZW = zw_add(n722, n486);
    let n725: ZW = zw_add(n723, n487);
    let n726: ZW = zw_add(n724, n490);
    let n727: ZW = zw_add(n725, n491);
    let n728: ZW = zw_add(n726, n604);
    let n729: ZW = zw_add(n727, n605);
    let n730: ZW = zw_add(n728, n608);
    let n731: ZW = zw_add(n729, n609);
    let n732: ZW = zw_add(n720, n612);
    let n733: ZW = zw_add(n721, n613);
    let n734: ZW = zw_add(n732, n486);
    let n735: ZW = zw_add(n733, n487);
    let n736: ZW = zw_add(n734, n502);
    let n737: ZW = zw_add(n735, n503);
    let n738: ZW = zw_add(n736, n620);
    let n739: ZW = zw_add(n737, n621);
    let n740: ZW = zw_add(n738, n608);
    let n741: ZW = zw_add(n739, n609);
    let n742: ZW = zw_add(n726, n626);
    let n743: ZW = zw_add(n727, n627);
    let n744: ZW = zw_add(n742, n608);
    let n745: ZW = zw_add(n743, n609);
    let n746: ZW = zw_add(n716, n632);
    let n747: ZW = zw_add(n717, n633);
    let n748: ZW = zw_add(n746, n636);
    let n749: ZW = zw_add(n747, n637);
    let n750: ZW = zw_add(n748, n482);
    let n751: ZW = zw_add(n749, n483);
    let n752: ZW = zw_add(n750, n642);
    let n753: ZW = zw_add(n751, n643);
    let n754: ZW = zw_add(n752, n490);
    let n755: ZW = zw_add(n753, n491);
    let n756: ZW = zw_add(n754, n494);
    let n757: ZW = zw_add(n755, n495);
    let n758: ZW = zw_add(n756, n650);
    let n759: ZW = zw_add(n757, n651);
    let n760: ZW = zw_add(n746, n592);
    let n761: ZW = zw_add(n747, n593);
    let n762: ZW = zw_add(n760, n612);
    let n763: ZW = zw_add(n761, n613);
    let n764: ZW = zw_add(n762, n642);
    let n765: ZW = zw_add(n763, n643);
    let n766: ZW = zw_add(n764, n502);
    let n767: ZW = zw_add(n765, n503);
    let n768: ZW = zw_add(n766, n662);
    let n769: ZW = zw_add(n767, n663);
    let n770: ZW = zw_add(n768, n666);
    let n771: ZW = zw_add(n769, n667);
    let n772: ZW = zw_add(n760, n596);
    let n773: ZW = zw_add(n761, n597);
    let n774: ZW = zw_add(n772, n642);
    let n775: ZW = zw_add(n773, n643);
    let n776: ZW = zw_add(n774, n490);
    let n777: ZW = zw_add(n775, n491);
    let n778: ZW = zw_add(n776, n676);
    let n779: ZW = zw_add(n777, n677);
    let n780: ZW = zw_add(n778, n666);
    let n781: ZW = zw_add(n779, n667);
    let n782: ZW = zw_add(n750, n682);
    let n783: ZW = zw_add(n751, n683);
    let n784: ZW = zw_add(n782, n490);
    let n785: ZW = zw_add(n783, n491);
    let n786: ZW = zw_add(n784, n494);
    let n787: ZW = zw_add(n785, n495);
    let n788: ZW = zw_add(n786, n690);
    let n789: ZW = zw_add(n787, n691);
    let n790: ZW = zw_add(n762, n682);
    let n791: ZW = zw_add(n763, n683);
    let n792: ZW = zw_add(n790, n502);
    let n793: ZW = zw_add(n791, n503);
    let n794: ZW = zw_add(n792, n662);
    let n795: ZW = zw_add(n793, n663);
    let n796: ZW = zw_add(n794, n700);
    let n797: ZW = zw_add(n795, n701);
    let n798: ZW = zw_add(n772, n682);
    let n799: ZW = zw_add(n773, n683);
    let n800: ZW = zw_add(n798, n490);
    let n801: ZW = zw_add(n799, n491);
    let n802: ZW = zw_add(n800, n676);
    let n803: ZW = zw_add(n801, n677);
    let n804: ZW = zw_add(n802, n700);
    let n805: ZW = zw_add(n803, n701);
    let n806: ZW = zw_add(zw_splat(0u64), n422);
    let n807: ZW = zw_add(zw_splat(0u64), n423);
    let n808: ZW = zw_add(n806, n426);
    let n809: ZW = zw_add(n807, n427);
    let n810: ZW = zw_add(n808, n430);
    let n811: ZW = zw_add(n809, n431);
    let n812: ZW = zw_cellmix_n(87u64, n349, 1542469173u64);
    let n813: ZW = zw_cellmix_n(87u64, n349, 668265263u64);
    let n814: ZW = zw_add(n810, n812);
    let n815: ZW = zw_add(n811, n813);
    let n816: ZW = zw_add(n814, n442);
    let n817: ZW = zw_add(n815, n443);
    let n818: ZW = zw_add(n816, n446);
    let n819: ZW = zw_add(n817, n447);
    let n820: ZW = zw_add(n814, n560);
    let n821: ZW = zw_add(n815, n561);
    let n822: ZW = zw_add(n820, n564);
    let n823: ZW = zw_add(n821, n565);
    let n824: ZW = zw_cellmix_b(38u64, n358, 1542469173u64);
    let n825: ZW = zw_cellmix_b(38u64, n358, 668265263u64);
    let n826: ZW = zw_add(zw_splat(0u64), n824);
    let n827: ZW = zw_add(zw_splat(0u64), n825);
    let n828: ZW = zw_cellmix_n(39u64, n357, 1542469173u64);
    let n829: ZW = zw_cellmix_n(39u64, n357, 668265263u64);
    let n830: ZW = zw_add(n826, n828);
    let n831: ZW = zw_add(n827, n829);
    let n832: ZW = zw_add(n830, n422);
    let n833: ZW = zw_add(n831, n423);
    let n834: ZW = zw_add(n832, n426);
    let n835: ZW = zw_add(n833, n427);
    let n836: ZW = zw_add(n834, n430);
    let n837: ZW = zw_add(n835, n431);
    let n838: ZW = zw_cellmix_n(87u64, n356, 1542469173u64);
    let n839: ZW = zw_cellmix_n(87u64, n356, 668265263u64);
    let n840: ZW = zw_add(n836, n838);
    let n841: ZW = zw_add(n837, n839);
    let n842: ZW = zw_add(n840, n442);
    let n843: ZW = zw_add(n841, n443);
    let n844: ZW = zw_add(n840, n560);
    let n845: ZW = zw_add(n841, n561);
    let n846: ZW = zw_cellmix_n(20u64, n369, 1542469173u64);
    let n847: ZW = zw_cellmix_n(20u64, n369, 668265263u64);
    let n848: ZW = zw_add(zw_splat(0u64), n846);
    let n849: ZW = zw_add(zw_splat(0u64), n847);
    let n850: ZW = zw_add(n848, n418);
    let n851: ZW = zw_add(n849, n419);
    let n852: ZW = zw_add(n850, n422);
    let n853: ZW = zw_add(n851, n423);
    let n854: ZW = zw_add(n852, n426);
    let n855: ZW = zw_add(n853, n427);
    let n856: ZW = zw_add(n854, n430);
    let n857: ZW = zw_add(n855, n431);
    let n858: ZW = zw_add(n856, n434);
    let n859: ZW = zw_add(n857, n435);
    let n860: ZW = zw_cellmix_n(252u64, n411, 1542469173u64);
    let n861: ZW = zw_cellmix_n(252u64, n411, 668265263u64);
    let n862: ZW = zw_add(n858, n860);
    let n863: ZW = zw_add(n859, n861);
    let n864: ZW = zw_cellmix_n(263u64, n386, 1542469173u64);
    let n865: ZW = zw_cellmix_n(263u64, n386, 668265263u64);
    let n866: ZW = zw_add(n862, n864);
    let n867: ZW = zw_add(n863, n865);
    let n868: ZW = zw_cellmix_n(264u64, n412, 1542469173u64);
    let n869: ZW = zw_cellmix_n(264u64, n412, 668265263u64);
    let n870: ZW = zw_add(n866, n868);
    let n871: ZW = zw_add(n867, n869);
    let n872: ZW = zw_cellmix_n(268u64, n413, 1542469173u64);
    let n873: ZW = zw_cellmix_n(268u64, n413, 668265263u64);
    let n874: ZW = zw_add(n870, n872);
    let n875: ZW = zw_add(n871, n873);
    let n876: ZW = zw_cellmix_n(294u64, n387, 1542469173u64);
    let n877: ZW = zw_cellmix_n(294u64, n387, 668265263u64);
    let n878: ZW = zw_add(n874, n876);
    let n879: ZW = zw_add(n875, n877);
    let n880: ZW = zw_cellmix_n(295u64, n388, 1542469173u64);
    let n881: ZW = zw_cellmix_n(295u64, n388, 668265263u64);
    let n882: ZW = zw_add(n878, n880);
    let n883: ZW = zw_add(n879, n881);
    let n884: ZW = zw_cellmix_n(309u64, n414, 1542469173u64);
    let n885: ZW = zw_cellmix_n(309u64, n414, 668265263u64);
    let n886: ZW = zw_add(n882, n884);
    let n887: ZW = zw_add(n883, n885);
    let n888: ZW = zw_cellmix_n(311u64, n415, 1542469173u64);
    let n889: ZW = zw_cellmix_n(311u64, n415, 668265263u64);
    let n890: ZW = zw_add(n886, n888);
    let n891: ZW = zw_add(n887, n889);
    let ok_v0_b0: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v0_b0: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v0_b0: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v1_b1: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v1_b1: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v1_b1: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v2_b2: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v2_b2: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v2_b2: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v16_b3: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v16_b3: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v16_b3: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v17_b4: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v17_b4: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v17_b4: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v18_b5: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v18_b5: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v18_b5: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v32_b6: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v32_b6: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v32_b6: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v33_b7: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v33_b7: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v33_b7: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v34_b8: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v34_b8: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v34_b8: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v36_b9: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v36_b9: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v36_b9: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v37_b10: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v37_b10: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v37_b10: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v38_b11: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v38_b11: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v38_b11: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v40_b12: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v40_b12: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v40_b12: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v41_b13: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v41_b13: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v41_b13: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v42_b14: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v42_b14: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v42_b14: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v48_b15: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v48_b15: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v48_b15: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v49_b16: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v49_b16: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v49_b16: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v50_b17: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v50_b17: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v50_b17: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v52_b18: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v52_b18: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v52_b18: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v53_b19: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v53_b19: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v53_b19: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v54_b20: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v54_b20: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v54_b20: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v56_b21: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v56_b21: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v56_b21: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v57_b22: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v57_b22: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v57_b22: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v58_b23: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n282);
    let bd_v58_b23: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v58_b23: u16 = ALL & zb_holds(n281) & zb_holds(n283) & zb_holds(n320);
    let ok_v0_b24: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v0_b24: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v0_b24: u16 = ALL & zb_holds(n320) & zb_holds(n350);
    let ok_v32_b25: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v32_b25: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v32_b25: u16 = ALL & zb_holds(n320) & zb_holds(n350);
    let ok_v0_b26: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n360);
    let bd_v0_b26: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v0_b26: u16 = ALL & zb_holds(n359);
    let ok_v32_b27: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n360);
    let bd_v32_b27: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v32_b27: u16 = ALL & zb_holds(n359);
    let ok_v0_b28: u16 = ALL & zb_holds(n97) & zb_holds(n96) & zb_holds(n95) & zb_holds(n94) & zb_holds(n93) & zb_holds(n91) & zb_holds(n90) & zb_holds(n89) & zb_holds(n84) & zb_holds(n83) & zb_holds(r_c251) & zb_holds(n82) & zb_holds(n81) & zb_holds(n80) & zb_holds(n79) & zb_holds(n78) & zb_holds(r_c243) & zb_holds(n77) & zb_holds(n76) & zb_holds(n73) & zb_holds(n72) & zb_holds(r_c234) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70);
    let bd_v0_b28: bool = !n88 || !n87 || !n86 || !n85 || !n75 || !n74;
    let live_v0_b28: u16 = ALL & zb_holds(n416);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n102,
        c86: n153,
        c273: n171,
        c85: n108,
    };
    let sh1 = KShared1 {
        c87: n349,
        c84: n102,
        c86: n153,
        c85: n108,
    };
    let sh2 = KShared2 {
        c87: n356,
        c39: n357,
        c84: n102,
        c86: n153,
        c85: n108,
        c38: n358,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: r_c39,
        c84: n102,
        c20: n369,
        c86: n153,
        c294: n387,
        c295: n388,
        c252: n411,
        c309: n414,
        c311: n415,
        c263: n386,
        c264: n412,
        c268: n413,
        c85: n108,
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
    let mut take_0_19: u16 = 0;
    let mut take_0_20: u16 = 0;
    let mut take_0_21: u16 = 0;
    let mut take_0_22: u16 = 0;
    let mut take_0_23: u16 = 0;
    let mut take_1_0: u16 = 0;
    let mut take_1_1: u16 = 0;
    let mut take_2_0: u16 = 0;
    let mut take_2_1: u16 = 0;
    let mut take_3_0: u16 = 0;
    // 29 distinct button assignments; per outcome they fall
    // into [24, 2, 2, 1] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(-65536i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: n302,
        h1: n500, h2: n501,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(-65536i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(true),
        c258: n293,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: n326,
        c321: n302,
        h1: n510, h2: n511,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(-65536i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: n329,
        c321: n302,
        h1: n516, h2: n517,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(-65536i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n317,
        c321: n318,
        h1: n544, h2: n545,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(-65536i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(true),
        c258: n316,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n328,
        c321: n318,
        h1: n552, h2: n553,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(-65536i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n331,
        c321: n318,
        h1: n558, h2: n559,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(65536i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n610, h2: n611,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n624, h2: n625,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n630, h2: n631,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(-327680i32)),
        h1: n652, h2: n653,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n668, h2: n669,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n680, h2: n681,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(327680i32)),
        h1: n692, h2: n693,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n702, h2: n703,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n293,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n710, h2: n711,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(65536i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n730, h2: n731,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n740, h2: n741,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(98304i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n744, h2: n745,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(-327680i32)),
        h1: n758, h2: n759,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n770, h2: n771,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(-98304i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n780, h2: n781,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(98304i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(327680i32)),
        h1: n788, h2: n789,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(-131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(true),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n796, h2: n797,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c308: zn_splat(P8::from_raw(69510i32)),
        c309: zn_splat(P8::from_raw(69510i32)),
        c253: zn_splat(P8::from_raw(655360i32)),
        c310: zn_splat(P8::from_raw(131072i32)),
        c311: zn_splat(P8::from_raw(131072i32)),
        c255: zn_splat(P8::from_raw(262144i32)),
        c256: zn_splat(P8::from_raw(0i32)),
        c312: zb_splat(false),
        c258: n316,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n804, h2: n805,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n818, h2: n819,
    };
    // body 24: buttons 0x00, forks 0x0
    sink.o1(0, take_1_0, &sh1, &o1);
    declined |= live_v32_b25 & (if bd_v32_b25 { ALL } else { !ok_v32_b25 });
    take_1_1 |= live_v32_b25 & ok_v32_b25 & (if bd_v32_b25 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: n822, h2: n823,
    };
    // body 25: buttons 0x20, forks 0x0
    sink.o1(32, take_1_1, &sh1, &o1);
    declined |= live_v0_b26 & (if bd_v0_b26 { ALL } else { !ok_v0_b26 });
    take_2_0 |= live_v0_b26 & ok_v0_b26 & (if bd_v0_b26 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: r_c20,
        h1: n842, h2: n843,
    };
    // body 26: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v32_b27 & (if bd_v32_b27 { ALL } else { !ok_v32_b27 });
    take_2_1 |= live_v32_b27 & ok_v32_b27 & (if bd_v32_b27 { 0 } else { ALL });
    let o2 = KOut2 {
        c20: zn_splat(P8::from_raw(131072i32)),
        h1: n844, h2: n845,
    };
    // body 27: buttons 0x20, forks 0x0
    sink.o2(32, take_2_1, &sh2, &o2);
    declined |= live_v0_b28 & (if bd_v0_b28 { ALL } else { !ok_v0_b28 });
    take_3_0 |= live_v0_b28 & ok_v0_b28 & (if bd_v0_b28 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n890, h2: n891,
    };
    // body 28: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
