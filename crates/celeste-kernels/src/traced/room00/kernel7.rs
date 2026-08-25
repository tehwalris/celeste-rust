// GENERATED from a TRACED frame (shape 7). Do not edit.
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
    pub c39: ZN,
    pub c273: ZN,
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
    pub c39: ZN,
    pub c20: ZN,
    pub c294: ZN,
    pub c295: ZN,
    pub c252: ZN,
    pub c309: ZN,
    pub c311: ZN,
    pub c263: ZN,
    pub c264: ZN,
    pub c268: ZN,
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
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[318] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[319] = Col::U(AV::Ival(P8::from_raw(-32768i32), P8::from_raw(32767i32)));
    b.cols[268] = Col::U(AV::Bool(true));
    b.cols[320] = Col::N(Vec::new());
    b.cols[321] = Col::N(Vec::new());
    b.cols[272] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[273] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[157] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[160] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
            } else { AV::UBool });
        }
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(true));
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
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
        if let Col::V(v) = &mut acc.cols[41] {
            v.push(if kv.c41.known & (1 << i) != 0 {
                AV::Bool(kv.c41.val & (1 << i) != 0)
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
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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
        if let Col::N(v) = &mut acc.cols[39] { v.push(kv.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(kv.c20.lane(i)); }
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

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[175] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[202] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[194] = Col::U(AV::Bool(true));
    b.cols[196] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[39] = Col::N(Vec::new());
    b.cols[188] = Col::U(AV::Bool(true));
    b.cols[189] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[178] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[184] = Col::U(AV::Bool(true));
    b.cols[186] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[86] = Col::U(AV::Num(P8::from_raw(0i32)));
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
    b.cols[85] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[38] = Col::U(AV::Bool(false));
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
        if let Col::N(v) = &mut acc.cols[294] { v.push(sh.c294.lane(i)); }
        if let Col::N(v) = &mut acc.cols[295] { v.push(sh.c295.lane(i)); }
        if let Col::N(v) = &mut acc.cols[252] { v.push(sh.c252.lane(i)); }
        if let Col::N(v) = &mut acc.cols[309] { v.push(sh.c309.lane(i)); }
        if let Col::N(v) = &mut acc.cols[311] { v.push(sh.c311.lane(i)); }
        if let Col::N(v) = &mut acc.cols[263] { v.push(sh.c263.lane(i)); }
        if let Col::N(v) = &mut acc.cols[264] { v.push(sh.c264.lane(i)); }
        if let Col::N(v) = &mut acc.cols[268] { v.push(sh.c268.lane(i)); }
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
    let n69: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c87);
    let n70: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c84);
    let n71: ZB = zb_not(r_c41);
    let n72: ZB = zb_not(r_c42);
    let n73: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n74: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c86);
    let n75: ZB = zb_not(r_c292);
    let n76: ZB = zb_not(r_c293);
    let n77: bool = P8::from_raw(0i32) == u.c296;
    let n78: bool = P8::from_raw(0i32) == u.c297;
    let n79: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c298);
    let n80: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c299);
    let n81: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c300);
    let n82: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c301);
    let n83: ZB = zn_eq(zn_splat(P8::from_raw(4194304i32)), r_c245);
    let n84: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c247);
    let n85: ZB = zn_eq(zn_splat(P8::from_raw(2097152i32)), r_c248);
    let n86: ZB = zb_not(r_c302);
    let n87: ZB = zb_not(r_c303);
    let n88: bool = P8::from_raw(524288i32) == u.c304;
    let n89: bool = P8::from_raw(524288i32) == u.c305;
    let n90: bool = P8::from_raw(0i32) == u.c306;
    let n91: bool = P8::from_raw(0i32) == u.c307;
    let n92: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c308);
    let n93: ZB = zb_not(r_c261);
    let n94: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c310);
    let n95: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c264);
    let n96: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c312);
    let n97: ZB = zn_eq(zn_splat(P8::from_raw(6291456i32)), r_c313);
    let n98: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c267);
    let n99: ZB = zb_not(r_c43);
    let n100: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c85);
    let n101: ZB = zb_not(r_c38);
    let n104: ZB = zb_not(n95);
    let n105: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c264);
    let n106: ZB = zb_not(n105);
    let n107: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c264);
    let n108: ZN = zn_sub(r_c252, zn_splat(P8::from_raw(65536i32)));
    let n109: ZB = zn_lt(n108, zn_splat(P8::from_raw(0i32)));
    let n122: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n124: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n150: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c311);
    let n151: ZB = zb_not(n150);
    let n152: ZB = zb_and(n104, n122);
    let n153: ZB = zb_and(n106, n152);
    let n154: ZB = zb_and(n107, n153);
    let n155: ZB = zb_and(n109, n154);
    let n164: ZN = zn_add(r_c309, r_c311);
    let n165: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n164);
    let n166: ZN = zn_flr(n165);
    let n167: ZN = zn_add(r_c268, n166);
    let n168: ZN = zsel_n(n151, n167, r_c268);
    let n169: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n168);
    let n170: ZN = zn_div(n169, zn_splat(P8::from_raw(524288i32)));
    let n171: ZN = zn_flr(n170);
    let n172: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n171);
    let n173: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n169);
    let n174: ZN = zn_sub(n173, zn_splat(P8::from_raw(65536i32)));
    let n175: ZN = zn_div(n174, zn_splat(P8::from_raw(524288i32)));
    let n176: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n175);
    let n177: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n172);
    let n178: ZB = zn_le(n177, n176);
    let n179: ZB = zn_gt(n177, n176);
    let n180: ZB = zb_and(n155, n178);
    let n181: ZB = zb_and(n155, n179);
    let n182: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n177);
    let n183: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n182);
    let n184: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n183);
    let n185: ZB = zb_not(n184);
    let n186: ZB = zb_and(n180, n184);
    let n187: ZB = zb_and(n180, n185);
    let n188: ZN = zn_rem(n174, zn_splat(P8::from_raw(524288i32)));
    let n189: ZB = zn_ge(n188, zn_splat(P8::from_raw(393216i32)));
    let n190: ZB = zn_lt(n188, zn_splat(P8::from_raw(393216i32)));
    let n191: ZB = zb_and(n186, n190);
    let n192: ZB = zb_and(n186, n189);
    let n193: ZN = zn_mul(n177, zn_splat(P8::from_raw(524288i32)));
    let n194: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n193);
    let n195: ZB = zn_eq(n173, n194);
    let n196: ZB = zb_or(n191, n192);
    let n197: ZB = zb_or(n189, n195);
    let n198: ZB = zb_or(n187, n196);
    let n199: ZB = zb_and(n184, n197);
    let n200: ZB = zb_not(n199);
    let n201: ZB = zb_and(n198, n199);
    let n202: ZB = zb_and(n198, n200);
    let n203: ZB = zb_or(n201, n202);
    let n204: ZB = zb_and(n200, n203);
    let n205: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n183);
    let n206: ZB = zb_not(n205);
    let n207: ZB = zb_and(n204, n205);
    let n208: ZB = zb_and(n204, n206);
    let n209: ZN = zn_rem(n169, zn_splat(P8::from_raw(524288i32)));
    let n210: ZB = zn_le(n209, zn_splat(P8::from_raw(131072i32)));
    let n211: ZB = zb_or(n207, n208);
    let n212: ZB = zb_and(n205, n210);
    let n213: ZB = zb_not(n212);
    let n214: ZB = zb_and(n211, n212);
    let n215: ZB = zb_and(n211, n213);
    let n216: ZB = zb_or(n214, n215);
    let n217: ZB = zb_and(n213, n216);
    let n218: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n183);
    let n219: ZB = zb_not(n218);
    let n220: ZB = zb_and(n217, n218);
    let n221: ZB = zb_and(n217, n219);
    let n222: ZB = zb_or(n220, n221);
    let n223: ZB = zb_and(n218, n222);
    let n224: ZB = zb_and(n219, n222);
    let n225: ZB = zb_or(n223, n224);
    let n226: ZB = zb_and(n219, n225);
    let n227: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n183);
    let n228: ZB = zb_not(n227);
    let n229: ZB = zb_and(n226, n227);
    let n230: ZB = zb_and(n226, n228);
    let n231: ZB = zb_or(n229, n230);
    let n232: ZB = zb_and(n227, n231);
    let n233: ZB = zb_and(n228, n231);
    let n234: ZB = zb_or(n232, n233);
    let n235: ZB = zb_and(n228, n234);
    let n236: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n172);
    let n237: ZB = zn_le(n236, n176);
    let n238: ZB = zn_gt(n236, n176);
    let n239: ZB = zb_and(n235, n237);
    let n240: ZB = zb_and(n235, n238);
    let n241: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n236);
    let n242: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n241);
    let n243: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n242);
    let n244: ZB = zb_not(n243);
    let n245: ZB = zb_and(n239, n243);
    let n246: ZB = zb_and(n239, n244);
    let n247: ZB = zb_and(n190, n245);
    let n248: ZB = zb_and(n189, n245);
    let n249: ZN = zn_mul(n236, zn_splat(P8::from_raw(524288i32)));
    let n250: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n249);
    let n251: ZB = zn_eq(n173, n250);
    let n252: ZB = zb_or(n247, n248);
    let n253: ZB = zb_or(n189, n251);
    let n254: ZB = zb_or(n246, n252);
    let n255: ZB = zb_and(n243, n253);
    let n256: ZB = zb_not(n255);
    let n257: ZB = zb_and(n254, n255);
    let n258: ZB = zb_and(n254, n256);
    let n259: ZB = zb_or(n257, n258);
    let n260: ZB = zb_and(n256, n259);
    let n261: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n242);
    let n262: ZB = zb_not(n261);
    let n263: ZB = zb_and(n260, n261);
    let n264: ZB = zb_and(n260, n262);
    let n265: ZB = zb_or(n263, n264);
    let n266: ZB = zb_and(n210, n261);
    let n267: ZB = zb_not(n266);
    let n268: ZB = zb_and(n265, n266);
    let n269: ZB = zb_and(n265, n267);
    let n270: ZB = zb_or(n268, n269);
    let n271: ZB = zb_and(n267, n270);
    let n272: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n242);
    let n273: ZB = zb_not(n272);
    let n274: ZB = zb_and(n271, n272);
    let n275: ZB = zb_and(n271, n273);
    let n276: ZB = zb_or(n274, n275);
    let n277: ZB = zb_and(n272, n276);
    let n278: ZB = zb_and(n273, n276);
    let n279: ZB = zb_or(n277, n278);
    let n280: ZB = zb_and(n273, n279);
    let n281: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n242);
    let n282: ZB = zb_not(n281);
    let n283: ZB = zb_and(n280, n281);
    let n284: ZB = zb_and(n280, n282);
    let n285: ZB = zb_or(n283, n284);
    let n286: ZB = zb_and(n281, n285);
    let n287: ZB = zb_and(n282, n285);
    let n288: ZB = zb_or(n286, n287);
    let n289: ZB = zb_and(n282, n288);
    let n290: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n172);
    let n291: ZB = zn_le(n290, n176);
    let n292: ZB = zn_gt(n290, n176);
    let n293: ZB = zb_and(n289, n291);
    let n294: ZB = zb_and(n289, n292);
    let n295: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n290);
    let n296: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(65536i32)), n295);
    let n297: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n296);
    let n298: ZB = zb_not(n297);
    let n299: ZB = zb_and(n293, n297);
    let n300: ZB = zb_and(n293, n298);
    let n301: ZB = zb_and(n190, n299);
    let n302: ZB = zb_and(n189, n299);
    let n303: ZN = zn_mul(n290, zn_splat(P8::from_raw(524288i32)));
    let n304: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n303);
    let n305: ZB = zn_eq(n173, n304);
    let n306: ZB = zb_or(n301, n302);
    let n307: ZB = zb_or(n189, n305);
    let n308: ZB = zb_or(n300, n306);
    let n309: ZB = zb_and(n297, n307);
    let n310: ZB = zb_not(n309);
    let n311: ZB = zb_and(n308, n309);
    let n312: ZB = zb_and(n308, n310);
    let n313: ZB = zb_or(n311, n312);
    let n314: ZB = zb_and(n310, n313);
    let n315: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n296);
    let n316: ZB = zb_not(n315);
    let n317: ZB = zb_and(n314, n315);
    let n318: ZB = zb_and(n314, n316);
    let n319: ZB = zb_or(n317, n318);
    let n320: ZB = zb_and(n210, n315);
    let n321: ZB = zb_not(n320);
    let n322: ZB = zb_and(n319, n320);
    let n323: ZB = zb_and(n319, n321);
    let n324: ZB = zb_or(n322, n323);
    let n325: ZB = zb_and(n321, n324);
    let n326: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n296);
    let n327: ZB = zb_not(n326);
    let n328: ZB = zb_and(n325, n326);
    let n329: ZB = zb_and(n325, n327);
    let n330: ZB = zb_or(n328, n329);
    let n331: ZB = zb_and(n326, n330);
    let n332: ZB = zb_and(n327, n330);
    let n333: ZB = zb_or(n331, n332);
    let n334: ZB = zb_and(n327, n333);
    let n335: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n296);
    let n336: ZB = zb_not(n335);
    let n337: ZB = zb_and(n334, n335);
    let n338: ZB = zb_and(n334, n336);
    let n339: ZB = zb_or(n337, n338);
    let n340: ZB = zb_and(n335, n339);
    let n341: ZB = zb_and(n336, n339);
    let n342: ZB = zb_or(n340, n341);
    let n343: ZB = zb_and(n336, n342);
    let n344: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n172);
    let n345: ZB = zn_gt(n344, n176);
    let n346: ZB = zb_or(n294, n343);
    let n347: ZB = zb_or(n292, n345);
    let n348: ZB = zb_or(n240, n346);
    let n349: ZB = zb_or(n238, n347);
    let n350: ZB = zb_or(n181, n348);
    let n351: ZB = zb_or(n179, n349);
    let n352: ZB = zn_le(n168, zn_splat(P8::from_raw(8388608i32)));
    let n353: ZB = zb_and(n350, n352);
    let n354: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n169);
    let n355: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n354, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n356: ZB = zb_not(n355);
    let n357: ZB = zb_and(n353, n356);
    let n358: ZB = zb_and(n353, n355);
    let n359: ZB = zb_or(n357, n358);
    let n360: ZB = zb_and(n356, n359);
    let n361: ZB = zb_and(n355, n359);
    let n362: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n354);
    let n363: ZB = zn_le(zn_splat(P8::from_raw(3145728i32)), n354);
    let n364: ZB = zb_and(n360, n362);
    let n365: ZB = zb_and(n360, n363);
    let n366: ZB = zb_or(n364, n365);
    let n367: ZB = zb_and(n362, n366);
    let n368: ZB = zb_and(n363, n366);
    let n369: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n173);
    let n370: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n369);
    let n371: ZB = zb_or(n367, n368);
    let n372: ZB = zb_and(n362, n370);
    let n373: ZB = zb_not(n372);
    let n374: ZB = zb_and(n371, n372);
    let n375: ZB = zb_and(n371, n373);
    let n376: ZB = zb_or(n374, n375);
    let n377: ZB = zb_or(n361, n376);
    let n378: ZB = zb_or(n355, n372);
    let n379: ZB = zb_not(n378);
    let n380: ZB = zb_and(n377, n378);
    let n381: ZB = zb_and(n377, n379);
    let n382: ZN = zsel_n(n378, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n383: ZB = zb_or(n380, n381);
    let n384: ZB = zb_and(n379, n383);
    let n385: ZB = zb_and(n378, n383);
    let n386: ZN = zsel_n(n379, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n387: ZB = zb_or(n384, n385);
    let n388: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n386);
    let n389: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n386);
    let n390: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n169);
    let n391: ZB = zn_gt(zn_splat(P8::from_raw(3145728i32)), n390);
    let n392: ZB = zn_le(zn_splat(P8::from_raw(3145728i32)), n390);
    let n393: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n173);
    let n394: ZB = zn_lt(zn_splat(P8::from_raw(2097152i32)), n393);
    let n395: ZB = zb_and(n391, n394);
    let n396: ZB = zb_not(n395);
    let n397: ZB = zb_and(n379, n387);
    let n398: ZB = zb_and(n378, n387);
    let n399: ZN = zsel_n(n379, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n400: ZB = zb_or(n397, n398);
    let n401: ZB = zn_gt(n382, zn_splat(P8::from_raw(0i32)));
    let n402: ZB = zn_le(n382, zn_splat(P8::from_raw(0i32)));
    let n403: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n390, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n404: ZB = zb_not(n403);
    let n405: ZB = zb_or(n395, n403);
    let n406: ZB = zb_not(n405);
    let n407: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n390, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n408: ZB = zb_not(n407);
    let n409: ZB = zb_or(n395, n407);
    let n410: ZB = zb_not(n409);
    let n411: ZN = zsel_n(n409, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n412: ZN = zsel_n(n405, zn_splat(P8::from_raw(-65536i32)), n411);
    let n413: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n412);
    let n414: ZB = zb_not(n413);
    let n415: ZN = zn_neg(n412);
    let n416: ZN = zn_mul(n415, zn_splat(P8::from_raw(131072i32)));
    let n417: ZN = zsel_n(n414, n416, zn_splat(P8::from_raw(0i32)));
    let n418: ZN = zsel_n(n414, zn_splat(P8::from_raw(-131072i32)), n399);
    let n419: ZN = zsel_n(n401, zn_splat(P8::from_raw(0i32)), n382);
    let n420: ZN = zsel_n(n401, zn_splat(P8::from_raw(0i32)), n417);
    let n421: ZN = zsel_n(n401, zn_splat(P8::from_raw(-131072i32)), n418);
    let n422: ZB = zn_lt(n168, zn_splat(P8::from_raw(-262144i32)));
    let n423: ZB = zn_ge(n168, zn_splat(P8::from_raw(-262144i32)));
    let n424: ZB = zb_and(n400, n422);
    let n425: ZB = zb_and(n400, n423);
    let n426: ZB = zb_or(n424, n425);
    let n432: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n388);
    let n433: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(524288i32)), n390, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n434: ZB = zb_not(n433);
    let n435: ZB = zb_and(n387, n434);
    let n436: ZB = zb_and(n387, n433);
    let n437: ZB = zb_or(n435, n436);
    let n438: ZB = zb_and(n434, n437);
    let n439: ZB = zb_and(n433, n437);
    let n440: ZB = zb_and(n391, n438);
    let n441: ZB = zb_and(n392, n438);
    let n442: ZB = zb_or(n440, n441);
    let n443: ZB = zb_and(n391, n442);
    let n444: ZB = zb_and(n392, n442);
    let n445: ZB = zb_or(n443, n444);
    let n446: ZB = zb_and(n395, n445);
    let n447: ZB = zb_and(n396, n445);
    let n448: ZB = zb_or(n446, n447);
    let n449: ZB = zb_or(n439, n448);
    let n450: ZB = zb_or(n395, n433);
    let n451: ZB = zb_not(n450);
    let n452: ZB = zb_and(n449, n450);
    let n453: ZB = zb_and(n449, n451);
    let n454: ZB = zb_or(n452, n453);
    let n455: ZB = zb_and(n450, n454);
    let n456: ZB = zb_and(n451, n454);
    let n457: ZB = zb_or(n455, n456);
    let n458: ZB = zb_and(n379, n457);
    let n459: ZB = zb_and(n378, n457);
    let n460: ZB = zb_or(n458, n459);
    let n461: ZN = zsel_n(n414, n416, n432);
    let n462: ZN = zsel_n(n401, n432, n461);
    let n463: ZB = zb_and(n422, n460);
    let n464: ZB = zb_and(n423, n460);
    let n465: ZB = zb_or(n463, n464);
    let n468: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n389);
    let n469: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(655360i32)), n390, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n470: ZB = zb_not(n469);
    let n471: ZB = zb_and(n387, n470);
    let n472: ZB = zb_and(n387, n469);
    let n473: ZB = zb_or(n471, n472);
    let n474: ZB = zb_and(n470, n473);
    let n475: ZB = zb_and(n469, n473);
    let n476: ZB = zb_and(n391, n474);
    let n477: ZB = zb_and(n392, n474);
    let n478: ZB = zb_or(n476, n477);
    let n479: ZB = zb_and(n391, n478);
    let n480: ZB = zb_and(n392, n478);
    let n481: ZB = zb_or(n479, n480);
    let n482: ZB = zb_and(n395, n481);
    let n483: ZB = zb_and(n396, n481);
    let n484: ZB = zb_or(n482, n483);
    let n485: ZB = zb_or(n475, n484);
    let n486: ZB = zb_or(n395, n469);
    let n487: ZB = zb_not(n486);
    let n488: ZB = zb_and(n485, n486);
    let n489: ZB = zb_and(n485, n487);
    let n490: ZB = zb_or(n488, n489);
    let n491: ZB = zb_and(n486, n490);
    let n492: ZB = zb_and(n487, n490);
    let n493: ZB = zb_or(n491, n492);
    let n494: ZB = zb_and(n379, n493);
    let n495: ZB = zb_and(n378, n493);
    let n496: ZB = zb_or(n494, n495);
    let n497: ZN = zsel_n(n414, n416, n468);
    let n498: ZN = zsel_n(n401, n468, n497);
    let n499: ZB = zb_and(n422, n496);
    let n500: ZB = zb_and(n423, n496);
    let n501: ZB = zb_or(n499, n500);
    let n504: ZB = zb_and(n400, n401);
    let n505: ZB = zb_and(n400, n402);
    let n506: ZB = zb_and(n404, n505);
    let n507: ZB = zb_and(n403, n505);
    let n508: ZB = zb_or(n506, n507);
    let n509: ZB = zb_and(n404, n508);
    let n510: ZB = zb_and(n403, n508);
    let n511: ZB = zb_and(n391, n509);
    let n512: ZB = zb_and(n392, n509);
    let n513: ZB = zb_or(n511, n512);
    let n514: ZB = zb_and(n391, n513);
    let n515: ZB = zb_and(n392, n513);
    let n516: ZB = zb_or(n514, n515);
    let n517: ZB = zb_and(n395, n516);
    let n518: ZB = zb_and(n396, n516);
    let n519: ZB = zb_or(n517, n518);
    let n520: ZB = zb_or(n510, n519);
    let n521: ZB = zb_and(n405, n520);
    let n522: ZB = zb_and(n406, n520);
    let n523: ZB = zb_and(n408, n522);
    let n524: ZB = zb_and(n407, n522);
    let n525: ZB = zb_or(n523, n524);
    let n526: ZB = zb_and(n408, n525);
    let n527: ZB = zb_and(n407, n525);
    let n528: ZB = zb_and(n391, n526);
    let n529: ZB = zb_and(n392, n526);
    let n530: ZB = zb_or(n528, n529);
    let n531: ZB = zb_and(n391, n530);
    let n532: ZB = zb_and(n392, n530);
    let n533: ZB = zb_or(n531, n532);
    let n534: ZB = zb_and(n396, n533);
    let n535: ZB = zb_or(n527, n534);
    let n536: ZB = zb_and(n409, n535);
    let n537: ZB = zb_and(n410, n535);
    let n538: ZB = zb_or(n536, n537);
    let n539: ZB = zb_or(n521, n538);
    let n540: ZB = zb_and(n414, n539);
    let n541: ZB = zb_and(n413, n539);
    let n542: ZB = zb_or(n540, n541);
    let n543: ZB = zb_or(n504, n542);
    let n544: ZB = zb_and(n422, n543);
    let n545: ZB = zb_and(n423, n543);
    let n546: ZB = zb_or(n544, n545);
    let n549: ZB = zb_and(n401, n460);
    let n550: ZB = zb_and(n402, n460);
    let n551: ZB = zb_and(n404, n550);
    let n552: ZB = zb_and(n403, n550);
    let n553: ZB = zb_or(n551, n552);
    let n554: ZB = zb_and(n404, n553);
    let n555: ZB = zb_and(n403, n553);
    let n556: ZB = zb_and(n391, n554);
    let n557: ZB = zb_and(n392, n554);
    let n558: ZB = zb_or(n556, n557);
    let n559: ZB = zb_and(n391, n558);
    let n560: ZB = zb_and(n392, n558);
    let n561: ZB = zb_or(n559, n560);
    let n562: ZB = zb_and(n395, n561);
    let n563: ZB = zb_and(n396, n561);
    let n564: ZB = zb_or(n562, n563);
    let n565: ZB = zb_or(n555, n564);
    let n566: ZB = zb_and(n405, n565);
    let n567: ZB = zb_and(n406, n565);
    let n568: ZB = zb_and(n408, n567);
    let n569: ZB = zb_and(n407, n567);
    let n570: ZB = zb_or(n568, n569);
    let n571: ZB = zb_and(n408, n570);
    let n572: ZB = zb_and(n407, n570);
    let n573: ZB = zb_and(n391, n571);
    let n574: ZB = zb_and(n392, n571);
    let n575: ZB = zb_or(n573, n574);
    let n576: ZB = zb_and(n391, n575);
    let n577: ZB = zb_and(n392, n575);
    let n578: ZB = zb_or(n576, n577);
    let n579: ZB = zb_and(n396, n578);
    let n580: ZB = zb_or(n572, n579);
    let n581: ZB = zb_and(n409, n580);
    let n582: ZB = zb_and(n410, n580);
    let n583: ZB = zb_or(n581, n582);
    let n584: ZB = zb_or(n566, n583);
    let n585: ZB = zb_and(n414, n584);
    let n586: ZB = zb_and(n413, n584);
    let n587: ZB = zb_or(n585, n586);
    let n588: ZB = zb_or(n549, n587);
    let n589: ZB = zb_and(n422, n588);
    let n590: ZB = zb_and(n423, n588);
    let n591: ZB = zb_or(n589, n590);
    let n594: ZB = zb_and(n401, n496);
    let n595: ZB = zb_and(n402, n496);
    let n596: ZB = zb_and(n404, n595);
    let n597: ZB = zb_and(n403, n595);
    let n598: ZB = zb_or(n596, n597);
    let n599: ZB = zb_and(n404, n598);
    let n600: ZB = zb_and(n403, n598);
    let n601: ZB = zb_and(n391, n599);
    let n602: ZB = zb_and(n392, n599);
    let n603: ZB = zb_or(n601, n602);
    let n604: ZB = zb_and(n391, n603);
    let n605: ZB = zb_and(n392, n603);
    let n606: ZB = zb_or(n604, n605);
    let n607: ZB = zb_and(n395, n606);
    let n608: ZB = zb_and(n396, n606);
    let n609: ZB = zb_or(n607, n608);
    let n610: ZB = zb_or(n600, n609);
    let n611: ZB = zb_and(n405, n610);
    let n612: ZB = zb_and(n406, n610);
    let n613: ZB = zb_and(n408, n612);
    let n614: ZB = zb_and(n407, n612);
    let n615: ZB = zb_or(n613, n614);
    let n616: ZB = zb_and(n408, n615);
    let n617: ZB = zb_and(n407, n615);
    let n618: ZB = zb_and(n391, n616);
    let n619: ZB = zb_and(n392, n616);
    let n620: ZB = zb_or(n618, n619);
    let n621: ZB = zb_and(n391, n620);
    let n622: ZB = zb_and(n392, n620);
    let n623: ZB = zb_or(n621, n622);
    let n624: ZB = zb_and(n396, n623);
    let n625: ZB = zb_or(n617, n624);
    let n626: ZB = zb_and(n409, n625);
    let n627: ZB = zb_and(n410, n625);
    let n628: ZB = zb_or(n626, n627);
    let n629: ZB = zb_or(n611, n628);
    let n630: ZB = zb_and(n414, n629);
    let n631: ZB = zb_and(n413, n629);
    let n632: ZB = zb_or(n630, n631);
    let n633: ZB = zb_or(n594, n632);
    let n634: ZB = zb_and(n422, n633);
    let n635: ZB = zb_and(n423, n633);
    let n636: ZB = zb_or(n634, n635);
    let n640: ZB = zb_and(n199, n203);
    let n641: ZB = zb_and(n212, n216);
    let n642: ZB = zb_and(n218, n225);
    let n643: ZB = zb_and(n227, n234);
    let n644: ZB = zb_or(n642, n643);
    let n645: ZB = zb_or(n641, n644);
    let n646: ZB = zb_or(n640, n645);
    let n647: ZB = zb_and(n255, n259);
    let n648: ZB = zb_and(n266, n270);
    let n649: ZB = zb_and(n272, n279);
    let n650: ZB = zb_and(n281, n288);
    let n651: ZB = zb_or(n649, n650);
    let n652: ZB = zb_or(n648, n651);
    let n653: ZB = zb_or(n647, n652);
    let n654: ZB = zb_and(n309, n313);
    let n655: ZB = zb_and(n320, n324);
    let n656: ZB = zb_and(n326, n333);
    let n657: ZB = zb_and(n335, n342);
    let n658: ZB = zb_or(n656, n657);
    let n659: ZB = zb_or(n655, n658);
    let n660: ZB = zb_or(n654, n659);
    let n661: ZB = zb_or(n653, n660);
    let n662: ZB = zb_or(n646, n661);
    let n663: ZB = zn_gt(n168, zn_splat(P8::from_raw(8388608i32)));
    let n664: ZB = zb_and(n662, n663);
    let n665: ZB = zb_and(n352, n662);
    let n666: ZB = zb_or(n664, n665);
    let n667: ZB = zb_and(n350, n663);
    let n668: ZB = zb_or(n666, n667);
    let n669: ZB = zb_or(n351, n666);
    let n670: ZB = zb_and(n356, n668);
    let n671: ZB = zb_and(n355, n668);
    let n672: ZB = zb_or(n670, n671);
    let n673: ZB = zb_and(n356, n672);
    let n674: ZB = zb_and(n355, n672);
    let n675: ZB = zb_and(n362, n673);
    let n676: ZB = zb_and(n363, n673);
    let n677: ZB = zb_or(n675, n676);
    let n678: ZB = zb_and(n362, n677);
    let n679: ZB = zb_and(n363, n677);
    let n680: ZB = zb_or(n678, n679);
    let n681: ZB = zb_and(n372, n680);
    let n682: ZB = zb_and(n373, n680);
    let n683: ZB = zb_or(n681, n682);
    let n684: ZB = zb_or(n674, n683);
    let n685: ZB = zb_and(n378, n684);
    let n686: ZB = zb_and(n379, n684);
    let n687: ZB = zb_or(n685, n686);
    let n688: ZB = zb_and(n379, n687);
    let n689: ZB = zb_and(n378, n687);
    let n690: ZB = zb_or(n688, n689);
    let n691: ZB = zb_and(n379, n690);
    let n692: ZB = zb_and(n378, n690);
    let n693: ZB = zb_or(n691, n692);
    let n694: ZB = zb_and(n422, n693);
    let n695: ZB = zb_and(n423, n693);
    let n696: ZB = zb_or(n694, n695);
    let n700: ZB = zb_and(n434, n690);
    let n701: ZB = zb_and(n433, n690);
    let n702: ZB = zb_or(n700, n701);
    let n703: ZB = zb_and(n434, n702);
    let n704: ZB = zb_and(n433, n702);
    let n705: ZB = zb_and(n391, n703);
    let n706: ZB = zb_and(n392, n703);
    let n707: ZB = zb_or(n705, n706);
    let n708: ZB = zb_and(n391, n707);
    let n709: ZB = zb_and(n392, n707);
    let n710: ZB = zb_or(n708, n709);
    let n711: ZB = zb_and(n395, n710);
    let n712: ZB = zb_and(n396, n710);
    let n713: ZB = zb_or(n711, n712);
    let n714: ZB = zb_or(n704, n713);
    let n715: ZB = zb_and(n450, n714);
    let n716: ZB = zb_and(n451, n714);
    let n717: ZB = zb_or(n715, n716);
    let n718: ZB = zb_and(n450, n717);
    let n719: ZB = zb_and(n451, n717);
    let n720: ZB = zb_or(n718, n719);
    let n721: ZB = zb_and(n379, n720);
    let n722: ZB = zb_and(n378, n720);
    let n723: ZB = zb_or(n721, n722);
    let n724: ZB = zb_and(n422, n723);
    let n725: ZB = zb_and(n423, n723);
    let n726: ZB = zb_or(n724, n725);
    let n729: ZB = zb_and(n470, n690);
    let n730: ZB = zb_and(n469, n690);
    let n731: ZB = zb_or(n729, n730);
    let n732: ZB = zb_and(n470, n731);
    let n733: ZB = zb_and(n469, n731);
    let n734: ZB = zb_and(n391, n732);
    let n735: ZB = zb_and(n392, n732);
    let n736: ZB = zb_or(n734, n735);
    let n737: ZB = zb_and(n391, n736);
    let n738: ZB = zb_and(n392, n736);
    let n739: ZB = zb_or(n737, n738);
    let n740: ZB = zb_and(n395, n739);
    let n741: ZB = zb_and(n396, n739);
    let n742: ZB = zb_or(n740, n741);
    let n743: ZB = zb_or(n733, n742);
    let n744: ZB = zb_and(n486, n743);
    let n745: ZB = zb_and(n487, n743);
    let n746: ZB = zb_or(n744, n745);
    let n747: ZB = zb_and(n486, n746);
    let n748: ZB = zb_and(n487, n746);
    let n749: ZB = zb_or(n747, n748);
    let n750: ZB = zb_and(n379, n749);
    let n751: ZB = zb_and(n378, n749);
    let n752: ZB = zb_or(n750, n751);
    let n753: ZB = zb_and(n422, n752);
    let n754: ZB = zb_and(n423, n752);
    let n755: ZB = zb_or(n753, n754);
    let n758: ZB = zb_and(n401, n693);
    let n759: ZB = zb_and(n402, n693);
    let n760: ZB = zb_and(n404, n759);
    let n761: ZB = zb_and(n403, n759);
    let n762: ZB = zb_or(n760, n761);
    let n763: ZB = zb_and(n404, n762);
    let n764: ZB = zb_and(n403, n762);
    let n765: ZB = zb_and(n391, n763);
    let n766: ZB = zb_and(n392, n763);
    let n767: ZB = zb_or(n765, n766);
    let n768: ZB = zb_and(n391, n767);
    let n769: ZB = zb_and(n392, n767);
    let n770: ZB = zb_or(n768, n769);
    let n771: ZB = zb_and(n395, n770);
    let n772: ZB = zb_and(n396, n770);
    let n773: ZB = zb_or(n771, n772);
    let n774: ZB = zb_or(n764, n773);
    let n775: ZB = zb_and(n405, n774);
    let n776: ZB = zb_and(n406, n774);
    let n777: ZB = zb_and(n408, n776);
    let n778: ZB = zb_and(n407, n776);
    let n779: ZB = zb_or(n777, n778);
    let n780: ZB = zb_and(n408, n779);
    let n781: ZB = zb_and(n407, n779);
    let n782: ZB = zb_and(n391, n780);
    let n783: ZB = zb_and(n392, n780);
    let n784: ZB = zb_or(n782, n783);
    let n785: ZB = zb_and(n391, n784);
    let n786: ZB = zb_and(n392, n784);
    let n787: ZB = zb_or(n785, n786);
    let n788: ZB = zb_and(n396, n787);
    let n789: ZB = zb_or(n781, n788);
    let n790: ZB = zb_and(n409, n789);
    let n791: ZB = zb_and(n410, n789);
    let n792: ZB = zb_or(n790, n791);
    let n793: ZB = zb_or(n775, n792);
    let n794: ZB = zb_and(n414, n793);
    let n795: ZB = zb_and(n413, n793);
    let n796: ZB = zb_or(n794, n795);
    let n797: ZB = zb_or(n758, n796);
    let n798: ZB = zb_and(n422, n797);
    let n799: ZB = zb_and(n423, n797);
    let n800: ZB = zb_or(n798, n799);
    let n803: ZB = zb_and(n401, n723);
    let n804: ZB = zb_and(n402, n723);
    let n805: ZB = zb_and(n404, n804);
    let n806: ZB = zb_and(n403, n804);
    let n807: ZB = zb_or(n805, n806);
    let n808: ZB = zb_and(n404, n807);
    let n809: ZB = zb_and(n403, n807);
    let n810: ZB = zb_and(n391, n808);
    let n811: ZB = zb_and(n392, n808);
    let n812: ZB = zb_or(n810, n811);
    let n813: ZB = zb_and(n391, n812);
    let n814: ZB = zb_and(n392, n812);
    let n815: ZB = zb_or(n813, n814);
    let n816: ZB = zb_and(n395, n815);
    let n817: ZB = zb_and(n396, n815);
    let n818: ZB = zb_or(n816, n817);
    let n819: ZB = zb_or(n809, n818);
    let n820: ZB = zb_and(n405, n819);
    let n821: ZB = zb_and(n406, n819);
    let n822: ZB = zb_and(n408, n821);
    let n823: ZB = zb_and(n407, n821);
    let n824: ZB = zb_or(n822, n823);
    let n825: ZB = zb_and(n408, n824);
    let n826: ZB = zb_and(n407, n824);
    let n827: ZB = zb_and(n391, n825);
    let n828: ZB = zb_and(n392, n825);
    let n829: ZB = zb_or(n827, n828);
    let n830: ZB = zb_and(n391, n829);
    let n831: ZB = zb_and(n392, n829);
    let n832: ZB = zb_or(n830, n831);
    let n833: ZB = zb_and(n396, n832);
    let n834: ZB = zb_or(n826, n833);
    let n835: ZB = zb_and(n409, n834);
    let n836: ZB = zb_and(n410, n834);
    let n837: ZB = zb_or(n835, n836);
    let n838: ZB = zb_or(n820, n837);
    let n839: ZB = zb_and(n414, n838);
    let n840: ZB = zb_and(n413, n838);
    let n841: ZB = zb_or(n839, n840);
    let n842: ZB = zb_or(n803, n841);
    let n843: ZB = zb_and(n422, n842);
    let n844: ZB = zb_and(n423, n842);
    let n845: ZB = zb_or(n843, n844);
    let n848: ZB = zb_and(n401, n752);
    let n849: ZB = zb_and(n402, n752);
    let n850: ZB = zb_and(n404, n849);
    let n851: ZB = zb_and(n403, n849);
    let n852: ZB = zb_or(n850, n851);
    let n853: ZB = zb_and(n404, n852);
    let n854: ZB = zb_and(n403, n852);
    let n855: ZB = zb_and(n391, n853);
    let n856: ZB = zb_and(n392, n853);
    let n857: ZB = zb_or(n855, n856);
    let n858: ZB = zb_and(n391, n857);
    let n859: ZB = zb_and(n392, n857);
    let n860: ZB = zb_or(n858, n859);
    let n861: ZB = zb_and(n395, n860);
    let n862: ZB = zb_and(n396, n860);
    let n863: ZB = zb_or(n861, n862);
    let n864: ZB = zb_or(n854, n863);
    let n865: ZB = zb_and(n405, n864);
    let n866: ZB = zb_and(n406, n864);
    let n867: ZB = zb_and(n408, n866);
    let n868: ZB = zb_and(n407, n866);
    let n869: ZB = zb_or(n867, n868);
    let n870: ZB = zb_and(n408, n869);
    let n871: ZB = zb_and(n407, n869);
    let n872: ZB = zb_and(n391, n870);
    let n873: ZB = zb_and(n392, n870);
    let n874: ZB = zb_or(n872, n873);
    let n875: ZB = zb_and(n391, n874);
    let n876: ZB = zb_and(n392, n874);
    let n877: ZB = zb_or(n875, n876);
    let n878: ZB = zb_and(n396, n877);
    let n879: ZB = zb_or(n871, n878);
    let n880: ZB = zb_and(n409, n879);
    let n881: ZB = zb_and(n410, n879);
    let n882: ZB = zb_or(n880, n881);
    let n883: ZB = zb_or(n865, n882);
    let n884: ZB = zb_and(n414, n883);
    let n885: ZB = zb_and(n413, n883);
    let n886: ZB = zb_or(n884, n885);
    let n887: ZB = zb_or(n848, n886);
    let n888: ZB = zb_and(n422, n887);
    let n889: ZB = zb_and(n423, n887);
    let n890: ZB = zb_or(n888, n889);
    let n894: ZB = zb_and(n422, n426);
    let n895: ZB = zb_and(n422, n696);
    let n896: ZN = zsel_n(n894, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n897: ZB = zb_not(n894);
    let n898: ZB = zb_or(n894, n895);
    let n899: ZB = zsel_b(n894, n351, n669);
    let n902: ZB = zb_and(n422, n465);
    let n903: ZB = zb_and(n422, n726);
    let n904: ZN = zsel_n(n902, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n905: ZB = zb_not(n902);
    let n906: ZB = zb_or(n902, n903);
    let n907: ZB = zsel_b(n902, n351, n669);
    let n910: ZB = zb_and(n422, n501);
    let n911: ZB = zb_and(n422, n755);
    let n912: ZN = zsel_n(n910, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n913: ZB = zb_not(n910);
    let n914: ZB = zb_or(n910, n911);
    let n915: ZB = zsel_b(n910, n351, n669);
    let n918: ZB = zb_and(n422, n546);
    let n919: ZB = zb_and(n422, n800);
    let n920: ZN = zsel_n(n918, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n921: ZB = zb_not(n918);
    let n922: ZB = zb_or(n918, n919);
    let n923: ZB = zsel_b(n918, n351, n669);
    let n926: ZB = zb_and(n422, n591);
    let n927: ZB = zb_and(n422, n845);
    let n928: ZN = zsel_n(n926, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n929: ZB = zb_not(n926);
    let n930: ZB = zb_or(n926, n927);
    let n931: ZB = zsel_b(n926, n351, n669);
    let n934: ZB = zb_and(n422, n636);
    let n935: ZB = zb_and(n422, n890);
    let n936: ZN = zsel_n(n934, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n937: ZB = zb_not(n934);
    let n938: ZB = zb_or(n934, n935);
    let n939: ZB = zsel_b(n934, n351, n669);
    let n945: ZB = zb_not(n107);
    let n946: ZB = zn_ge(n108, zn_splat(P8::from_raw(0i32)));
    let n947: ZN = zsel_n(n107, n108, r_c252);
    let n948: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n949: ZN = zsel_n(n124, n948, r_c20);
    let n950: ZB = zn_gt(n949, zn_splat(P8::from_raw(0i32)));
    let n951: ZB = zn_le(n949, zn_splat(P8::from_raw(0i32)));
    let n952: ZB = zb_and(n95, n122);
    let n953: ZB = zb_and(n105, n152);
    let n954: ZN = zn_add(r_c311, zn_splat(P8::from_raw(32768i32)));
    let n955: ZB = zn_gt(n954, zn_splat(P8::from_raw(0i32)));
    let n956: ZB = zn_le(n954, zn_splat(P8::from_raw(0i32)));
    let n957: ZB = zb_and(n953, n955);
    let n958: ZB = zb_and(n953, n956);
    let n959: ZB = zn_gt(r_c252, zn_splat(P8::from_raw(0i32)));
    let n960: ZB = zb_or(n957, n958);
    let n961: ZB = zb_and(n955, n959);
    let n962: ZB = zb_not(n961);
    let n963: ZB = zb_and(n960, n961);
    let n964: ZB = zb_and(n960, n962);
    let n965: ZN = zsel_n(n961, n108, r_c252);
    let n966: ZN = zsel_n(n961, zn_splat(P8::from_raw(0i32)), n954);
    let n967: ZB = zb_or(n963, n964);
    let n968: ZB = zn_gt(n966, zn_splat(P8::from_raw(0i32)));
    let n969: ZB = zn_le(n966, zn_splat(P8::from_raw(0i32)));
    let n970: ZB = zb_and(n967, n968);
    let n971: ZB = zb_and(n967, n969);
    let n972: ZB = zb_or(n970, n971);
    let n973: ZB = zb_and(n153, n945);
    let n974: ZB = zb_and(n154, n946);
    let n975: ZN = zsel_n(n107, zn_splat(P8::from_raw(393216i32)), r_c263);
    let n976: ZB = zb_or(n973, n974);
    let n977: ZN = zsel_n(n105, r_c263, n975);
    let n978: ZN = zsel_n(n95, r_c263, n977);
    let n979: ZN = zsel_n(n124, r_c263, n978);
    let n980: ZN = zsel_n(n124, zn_splat(u.c294), zn_splat(P8::from_raw(1048576i32)));
    let n981: ZN = zsel_n(n124, zn_splat(u.c295), zn_splat(P8::from_raw(1048576i32)));
    let n982: ZN = zn_sub(n165, zn_splat(P8::from_raw(32768i32)));
    let n983: ZN = zn_sub(n982, n166);
    let n984: ZN = zsel_n(n151, n983, r_c309);
    let n985: ZB = zn_lt(n168, zn_splat(P8::from_raw(7340032i32)));
    let n986: ZB = zn_ge(n168, zn_splat(P8::from_raw(7340032i32)));
    let n987: ZB = zb_and(n952, n985);
    let n988: ZB = zb_and(n952, n986);
    let n989: ZN = zsel_n(n985, zn_splat(P8::from_raw(196608i32)), r_c252);
    let n990: ZN = zsel_n(n985, zn_splat(P8::from_raw(65536i32)), r_c264);
    let n991: ZB = zb_or(n987, n988);
    let n992: ZB = zn_gt(n168, zn_splat(P8::from_raw(6291456i32)));
    let n993: ZB = zb_and(n968, n992);
    let n994: ZB = zb_not(n993);
    let n995: ZB = zb_and(n972, n993);
    let n996: ZB = zb_and(n972, n994);
    let n997: ZN = zsel_n(n993, zn_splat(P8::from_raw(327680i32)), n965);
    let n998: ZN = zsel_n(n993, zn_splat(P8::from_raw(131072i32)), r_c264);
    let n999: ZN = zsel_n(n993, zn_splat(P8::from_raw(6291456i32)), n168);
    let n1000: ZN = zsel_n(n993, zn_splat(P8::from_raw(0i32)), n966);
    let n1001: ZB = zb_or(n995, n996);
    let n1002: ZN = zsel_n(n105, n997, n947);
    let n1003: ZN = zsel_n(n105, n998, r_c264);
    let n1004: ZN = zsel_n(n105, n999, n168);
    let n1005: ZN = zsel_n(n105, n1000, r_c311);
    let n1006: ZB = zb_or(n976, n1001);
    let n1007: ZN = zsel_n(n95, n989, n1002);
    let n1008: ZN = zsel_n(n95, n990, n1003);
    let n1009: ZN = zsel_n(n95, n168, n1004);
    let n1010: ZN = zsel_n(n95, r_c311, n1005);
    let n1011: ZB = zb_or(n991, n1006);
    let n1012: ZN = zsel_n(n124, r_c252, n1007);
    let n1013: ZN = zsel_n(n124, r_c264, n1008);
    let n1014: ZN = zsel_n(n124, r_c268, n1009);
    let n1015: ZN = zsel_n(n124, r_c309, n984);
    let n1016: ZN = zsel_n(n124, r_c311, n1010);
    let n1017: ZB = zb_or(n124, n1011);
    let n1018: ZB = zb_and(n950, n1017);
    let n1019: ZB = zb_and(n951, n1017);
    let n1020: ZB = zb_or(n1018, n1019);
    let n1023: ZW = zw_bits_n(r_c39);
    let n1024: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1023, 39u64);
    let n1025: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1023, 39u64);
    let n1026: ZW = zw_bits_n(n168);
    let n1027: ZW = zw_mix1(n1024, n1026, 273u64);
    let n1028: ZW = zw_mix2(n1025, n1026, 273u64);
    let n1029: ZW = zw_bits_n(r_c20);
    let n1030: ZW = zw_mix1(n1027, n1029, 20u64);
    let n1031: ZW = zw_mix2(n1028, n1029, 20u64);
    let n1032: u64 = false as u64;
    let n1033: ZW = zw_mix1(n1030, zw_splat(n1032), 41u64);
    let n1034: ZW = zw_mix2(n1031, zw_splat(n1032), 41u64);
    let n1035: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n1036: ZW = zw_mix1(n1033, zw_splat(n1035), 253u64);
    let n1037: ZW = zw_mix2(n1034, zw_splat(n1035), 253u64);
    let n1038: ZW = zw_mix1(n1036, zw_splat(n1035), 255u64);
    let n1039: ZW = zw_mix2(n1037, zw_splat(n1035), 255u64);
    let n1040: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n1041: ZW = zw_mix1(n1038, zw_splat(n1040), 256u64);
    let n1042: ZW = zw_mix2(n1039, zw_splat(n1040), 256u64);
    let n1043: ZW = zw_bits_n(n382);
    let n1044: ZW = zw_mix1(n1041, n1043, 258u64);
    let n1045: ZW = zw_mix2(n1042, n1043, 258u64);
    let n1046: ZW = zw_mix1(n1044, zw_splat(n1032), 265u64);
    let n1047: ZW = zw_mix2(n1045, zw_splat(n1032), 265u64);
    let n1048: ZW = zw_mix1(n1046, zw_splat(n1032), 266u64);
    let n1049: ZW = zw_mix2(n1047, zw_splat(n1032), 266u64);
    let n1050: ZW = zw_mix1(n1048, zw_splat(n1035), 308u64);
    let n1051: ZW = zw_mix2(n1049, zw_splat(n1035), 308u64);
    let n1052: ZW = zw_mix1(n1050, zw_splat(n1035), 309u64);
    let n1053: ZW = zw_mix2(n1051, zw_splat(n1035), 309u64);
    let n1054: ZW = zw_mix1(n1052, zw_splat(n1035), 310u64);
    let n1055: ZW = zw_mix2(n1053, zw_splat(n1035), 310u64);
    let n1056: ZW = zw_mix1(n1054, zw_splat(n1035), 311u64);
    let n1057: ZW = zw_mix2(n1055, zw_splat(n1035), 311u64);
    let n1058: ZW = zw_mix1(n1056, zw_splat(n1032), 312u64);
    let n1059: ZW = zw_mix2(n1057, zw_splat(n1032), 312u64);
    let n1060: ZW = zw_mix1(n1058, zw_splat(n1035), 320u64);
    let n1061: ZW = zw_mix2(n1059, zw_splat(n1035), 320u64);
    let n1062: ZW = zw_bits_n(n399);
    let n1063: ZW = zw_mix1(n1060, n1062, 321u64);
    let n1064: ZW = zw_mix2(n1061, n1062, 321u64);
    let n1065: u64 = true as u64;
    let n1066: ZW = zw_mix1(n1056, zw_splat(n1065), 312u64);
    let n1067: ZW = zw_mix2(n1057, zw_splat(n1065), 312u64);
    let n1068: ZW = zw_bits_n(n432);
    let n1069: ZW = zw_mix1(n1066, n1068, 320u64);
    let n1070: ZW = zw_mix2(n1067, n1068, 320u64);
    let n1071: ZW = zw_mix1(n1069, n1062, 321u64);
    let n1072: ZW = zw_mix2(n1070, n1062, 321u64);
    let n1073: ZW = zw_bits_n(n468);
    let n1074: ZW = zw_mix1(n1058, n1073, 320u64);
    let n1075: ZW = zw_mix2(n1059, n1073, 320u64);
    let n1076: ZW = zw_mix1(n1074, n1062, 321u64);
    let n1077: ZW = zw_mix2(n1075, n1062, 321u64);
    let n1078: ZW = zw_bits_n(n419);
    let n1079: ZW = zw_mix1(n1041, n1078, 258u64);
    let n1080: ZW = zw_mix2(n1042, n1078, 258u64);
    let n1081: ZW = zw_mix1(n1079, zw_splat(n1032), 265u64);
    let n1082: ZW = zw_mix2(n1080, zw_splat(n1032), 265u64);
    let n1083: ZW = zw_mix1(n1081, zw_splat(n1065), 266u64);
    let n1084: ZW = zw_mix2(n1082, zw_splat(n1065), 266u64);
    let n1085: ZW = zw_mix1(n1083, zw_splat(n1035), 308u64);
    let n1086: ZW = zw_mix2(n1084, zw_splat(n1035), 308u64);
    let n1087: ZW = zw_mix1(n1085, zw_splat(n1035), 309u64);
    let n1088: ZW = zw_mix2(n1086, zw_splat(n1035), 309u64);
    let n1089: ZW = zw_mix1(n1087, zw_splat(n1035), 310u64);
    let n1090: ZW = zw_mix2(n1088, zw_splat(n1035), 310u64);
    let n1091: ZW = zw_mix1(n1089, zw_splat(n1035), 311u64);
    let n1092: ZW = zw_mix2(n1090, zw_splat(n1035), 311u64);
    let n1093: ZW = zw_mix1(n1091, zw_splat(n1032), 312u64);
    let n1094: ZW = zw_mix2(n1092, zw_splat(n1032), 312u64);
    let n1095: ZW = zw_bits_n(n420);
    let n1096: ZW = zw_mix1(n1093, n1095, 320u64);
    let n1097: ZW = zw_mix2(n1094, n1095, 320u64);
    let n1098: ZW = zw_bits_n(n421);
    let n1099: ZW = zw_mix1(n1096, n1098, 321u64);
    let n1100: ZW = zw_mix2(n1097, n1098, 321u64);
    let n1101: ZW = zw_mix1(n1091, zw_splat(n1065), 312u64);
    let n1102: ZW = zw_mix2(n1092, zw_splat(n1065), 312u64);
    let n1103: ZW = zw_bits_n(n462);
    let n1104: ZW = zw_mix1(n1101, n1103, 320u64);
    let n1105: ZW = zw_mix2(n1102, n1103, 320u64);
    let n1106: ZW = zw_mix1(n1104, n1098, 321u64);
    let n1107: ZW = zw_mix2(n1105, n1098, 321u64);
    let n1108: ZW = zw_bits_n(n498);
    let n1109: ZW = zw_mix1(n1093, n1108, 320u64);
    let n1110: ZW = zw_mix2(n1094, n1108, 320u64);
    let n1111: ZW = zw_mix1(n1109, n1098, 321u64);
    let n1112: ZW = zw_mix2(n1110, n1098, 321u64);
    let n1113: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n1114: ZW = zw_mix1(n1027, zw_splat(n1113), 20u64);
    let n1115: ZW = zw_mix2(n1028, zw_splat(n1113), 20u64);
    let n1116: ZW = zw_mix1(n1114, zw_splat(n1065), 41u64);
    let n1117: ZW = zw_mix2(n1115, zw_splat(n1065), 41u64);
    let n1118: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n1119: ZW = zw_mix1(n1116, zw_splat(n1118), 253u64);
    let n1120: ZW = zw_mix2(n1117, zw_splat(n1118), 253u64);
    let n1121: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n1122: ZW = zw_mix1(n1119, zw_splat(n1121), 255u64);
    let n1123: ZW = zw_mix2(n1120, zw_splat(n1121), 255u64);
    let n1124: ZW = zw_mix1(n1122, zw_splat(n1035), 256u64);
    let n1125: ZW = zw_mix2(n1123, zw_splat(n1035), 256u64);
    let n1126: ZW = zw_mix1(n1124, n1043, 258u64);
    let n1127: ZW = zw_mix2(n1125, n1043, 258u64);
    let n1128: ZW = zw_mix1(n1126, zw_splat(n1065), 265u64);
    let n1129: ZW = zw_mix2(n1127, zw_splat(n1065), 265u64);
    let n1130: ZW = zw_mix1(n1128, zw_splat(n1032), 266u64);
    let n1131: ZW = zw_mix2(n1129, zw_splat(n1032), 266u64);
    let n1132: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n1133: ZW = zw_mix1(n1130, zw_splat(n1132), 308u64);
    let n1134: ZW = zw_mix2(n1131, zw_splat(n1132), 308u64);
    let n1135: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n1136: ZW = zw_mix1(n1133, zw_splat(n1135), 309u64);
    let n1137: ZW = zw_mix2(n1134, zw_splat(n1135), 309u64);
    let n1138: ZW = zw_mix1(n1136, zw_splat(n1113), 310u64);
    let n1139: ZW = zw_mix2(n1137, zw_splat(n1113), 310u64);
    let n1140: ZW = zw_mix1(n1138, zw_splat(n1035), 311u64);
    let n1141: ZW = zw_mix2(n1139, zw_splat(n1035), 311u64);
    let n1142: ZW = zw_mix1(n1140, zw_splat(n1032), 312u64);
    let n1143: ZW = zw_mix2(n1141, zw_splat(n1032), 312u64);
    let n1144: ZW = zw_mix1(n1142, zw_splat(n1040), 320u64);
    let n1145: ZW = zw_mix2(n1143, zw_splat(n1040), 320u64);
    let n1146: ZW = zw_mix1(n1144, zw_splat(n1035), 321u64);
    let n1147: ZW = zw_mix2(n1145, zw_splat(n1035), 321u64);
    let n1148: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n1149: ZW = zw_mix1(n1136, zw_splat(n1148), 310u64);
    let n1150: ZW = zw_mix2(n1137, zw_splat(n1148), 310u64);
    let n1151: ZW = zw_mix1(n1149, zw_splat(n1035), 311u64);
    let n1152: ZW = zw_mix2(n1150, zw_splat(n1035), 311u64);
    let n1153: ZW = zw_mix1(n1151, zw_splat(n1065), 312u64);
    let n1154: ZW = zw_mix2(n1152, zw_splat(n1065), 312u64);
    let n1155: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n1156: ZW = zw_mix1(n1153, zw_splat(n1155), 320u64);
    let n1157: ZW = zw_mix2(n1154, zw_splat(n1155), 320u64);
    let n1158: ZW = zw_mix1(n1156, zw_splat(n1035), 321u64);
    let n1159: ZW = zw_mix2(n1157, zw_splat(n1035), 321u64);
    let n1160: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n1161: ZW = zw_mix1(n1142, zw_splat(n1160), 320u64);
    let n1162: ZW = zw_mix2(n1143, zw_splat(n1160), 320u64);
    let n1163: ZW = zw_mix1(n1161, zw_splat(n1035), 321u64);
    let n1164: ZW = zw_mix2(n1162, zw_splat(n1035), 321u64);
    let n1165: ZW = zw_mix1(n1130, zw_splat(n1135), 308u64);
    let n1166: ZW = zw_mix2(n1131, zw_splat(n1135), 308u64);
    let n1167: ZW = zw_mix1(n1165, zw_splat(n1132), 309u64);
    let n1168: ZW = zw_mix2(n1166, zw_splat(n1132), 309u64);
    let n1169: ZW = zw_mix1(n1167, zw_splat(n1035), 310u64);
    let n1170: ZW = zw_mix2(n1168, zw_splat(n1035), 310u64);
    let n1171: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n1172: ZW = zw_mix1(n1169, zw_splat(n1171), 311u64);
    let n1173: ZW = zw_mix2(n1170, zw_splat(n1171), 311u64);
    let n1174: ZW = zw_mix1(n1172, zw_splat(n1032), 312u64);
    let n1175: ZW = zw_mix2(n1173, zw_splat(n1032), 312u64);
    let n1176: ZW = zw_mix1(n1174, zw_splat(n1035), 320u64);
    let n1177: ZW = zw_mix2(n1175, zw_splat(n1035), 320u64);
    let n1178: ZW = zw_mix1(n1176, zw_splat(n1155), 321u64);
    let n1179: ZW = zw_mix2(n1177, zw_splat(n1155), 321u64);
    let n1180: ZW = zw_mix1(n1165, zw_splat(n1135), 309u64);
    let n1181: ZW = zw_mix2(n1166, zw_splat(n1135), 309u64);
    let n1182: ZW = zw_mix1(n1180, zw_splat(n1148), 310u64);
    let n1183: ZW = zw_mix2(n1181, zw_splat(n1148), 310u64);
    let n1184: ZW = zw_mix1(n1182, zw_splat(n1171), 311u64);
    let n1185: ZW = zw_mix2(n1183, zw_splat(n1171), 311u64);
    let n1186: ZW = zw_mix1(n1184, zw_splat(n1065), 312u64);
    let n1187: ZW = zw_mix2(n1185, zw_splat(n1065), 312u64);
    let n1188: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n1189: ZW = zw_mix1(n1186, zw_splat(n1188), 320u64);
    let n1190: ZW = zw_mix2(n1187, zw_splat(n1188), 320u64);
    let n1191: ZW = zw_mix1(n1189, zw_splat(n1188), 321u64);
    let n1192: ZW = zw_mix2(n1190, zw_splat(n1188), 321u64);
    let n1193: ZW = zw_mix1(n1180, zw_splat(n1113), 310u64);
    let n1194: ZW = zw_mix2(n1181, zw_splat(n1113), 310u64);
    let n1195: ZW = zw_mix1(n1193, zw_splat(n1171), 311u64);
    let n1196: ZW = zw_mix2(n1194, zw_splat(n1171), 311u64);
    let n1197: ZW = zw_mix1(n1195, zw_splat(n1032), 312u64);
    let n1198: ZW = zw_mix2(n1196, zw_splat(n1032), 312u64);
    let n1199: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n1200: ZW = zw_mix1(n1197, zw_splat(n1199), 320u64);
    let n1201: ZW = zw_mix2(n1198, zw_splat(n1199), 320u64);
    let n1202: ZW = zw_mix1(n1200, zw_splat(n1188), 321u64);
    let n1203: ZW = zw_mix2(n1201, zw_splat(n1188), 321u64);
    let n1204: ZW = zw_mix1(n1169, zw_splat(n1113), 311u64);
    let n1205: ZW = zw_mix2(n1170, zw_splat(n1113), 311u64);
    let n1206: ZW = zw_mix1(n1204, zw_splat(n1032), 312u64);
    let n1207: ZW = zw_mix2(n1205, zw_splat(n1032), 312u64);
    let n1208: ZW = zw_mix1(n1206, zw_splat(n1035), 320u64);
    let n1209: ZW = zw_mix2(n1207, zw_splat(n1035), 320u64);
    let n1210: ZW = zw_mix1(n1208, zw_splat(n1160), 321u64);
    let n1211: ZW = zw_mix2(n1209, zw_splat(n1160), 321u64);
    let n1212: ZW = zw_mix1(n1182, zw_splat(n1113), 311u64);
    let n1213: ZW = zw_mix2(n1183, zw_splat(n1113), 311u64);
    let n1214: ZW = zw_mix1(n1212, zw_splat(n1065), 312u64);
    let n1215: ZW = zw_mix2(n1213, zw_splat(n1065), 312u64);
    let n1216: ZW = zw_mix1(n1214, zw_splat(n1188), 320u64);
    let n1217: ZW = zw_mix2(n1215, zw_splat(n1188), 320u64);
    let n1218: ZW = zw_mix1(n1216, zw_splat(n1199), 321u64);
    let n1219: ZW = zw_mix2(n1217, zw_splat(n1199), 321u64);
    let n1220: ZW = zw_mix1(n1193, zw_splat(n1113), 311u64);
    let n1221: ZW = zw_mix2(n1194, zw_splat(n1113), 311u64);
    let n1222: ZW = zw_mix1(n1220, zw_splat(n1032), 312u64);
    let n1223: ZW = zw_mix2(n1221, zw_splat(n1032), 312u64);
    let n1224: ZW = zw_mix1(n1222, zw_splat(n1199), 320u64);
    let n1225: ZW = zw_mix2(n1223, zw_splat(n1199), 320u64);
    let n1226: ZW = zw_mix1(n1224, zw_splat(n1199), 321u64);
    let n1227: ZW = zw_mix2(n1225, zw_splat(n1199), 321u64);
    let n1228: ZW = zw_mix1(n1124, n1078, 258u64);
    let n1229: ZW = zw_mix2(n1125, n1078, 258u64);
    let n1230: ZW = zw_mix1(n1228, zw_splat(n1065), 265u64);
    let n1231: ZW = zw_mix2(n1229, zw_splat(n1065), 265u64);
    let n1232: ZW = zw_mix1(n1230, zw_splat(n1065), 266u64);
    let n1233: ZW = zw_mix2(n1231, zw_splat(n1065), 266u64);
    let n1234: ZW = zw_mix1(n1232, zw_splat(n1132), 308u64);
    let n1235: ZW = zw_mix2(n1233, zw_splat(n1132), 308u64);
    let n1236: ZW = zw_mix1(n1234, zw_splat(n1135), 309u64);
    let n1237: ZW = zw_mix2(n1235, zw_splat(n1135), 309u64);
    let n1238: ZW = zw_mix1(n1236, zw_splat(n1113), 310u64);
    let n1239: ZW = zw_mix2(n1237, zw_splat(n1113), 310u64);
    let n1240: ZW = zw_mix1(n1238, zw_splat(n1035), 311u64);
    let n1241: ZW = zw_mix2(n1239, zw_splat(n1035), 311u64);
    let n1242: ZW = zw_mix1(n1240, zw_splat(n1032), 312u64);
    let n1243: ZW = zw_mix2(n1241, zw_splat(n1032), 312u64);
    let n1244: ZW = zw_mix1(n1242, zw_splat(n1040), 320u64);
    let n1245: ZW = zw_mix2(n1243, zw_splat(n1040), 320u64);
    let n1246: ZW = zw_mix1(n1244, zw_splat(n1035), 321u64);
    let n1247: ZW = zw_mix2(n1245, zw_splat(n1035), 321u64);
    let n1248: ZW = zw_mix1(n1236, zw_splat(n1148), 310u64);
    let n1249: ZW = zw_mix2(n1237, zw_splat(n1148), 310u64);
    let n1250: ZW = zw_mix1(n1248, zw_splat(n1035), 311u64);
    let n1251: ZW = zw_mix2(n1249, zw_splat(n1035), 311u64);
    let n1252: ZW = zw_mix1(n1250, zw_splat(n1065), 312u64);
    let n1253: ZW = zw_mix2(n1251, zw_splat(n1065), 312u64);
    let n1254: ZW = zw_mix1(n1252, zw_splat(n1155), 320u64);
    let n1255: ZW = zw_mix2(n1253, zw_splat(n1155), 320u64);
    let n1256: ZW = zw_mix1(n1254, zw_splat(n1035), 321u64);
    let n1257: ZW = zw_mix2(n1255, zw_splat(n1035), 321u64);
    let n1258: ZW = zw_mix1(n1242, zw_splat(n1160), 320u64);
    let n1259: ZW = zw_mix2(n1243, zw_splat(n1160), 320u64);
    let n1260: ZW = zw_mix1(n1258, zw_splat(n1035), 321u64);
    let n1261: ZW = zw_mix2(n1259, zw_splat(n1035), 321u64);
    let n1262: ZW = zw_mix1(n1232, zw_splat(n1135), 308u64);
    let n1263: ZW = zw_mix2(n1233, zw_splat(n1135), 308u64);
    let n1264: ZW = zw_mix1(n1262, zw_splat(n1132), 309u64);
    let n1265: ZW = zw_mix2(n1263, zw_splat(n1132), 309u64);
    let n1266: ZW = zw_mix1(n1264, zw_splat(n1035), 310u64);
    let n1267: ZW = zw_mix2(n1265, zw_splat(n1035), 310u64);
    let n1268: ZW = zw_mix1(n1266, zw_splat(n1171), 311u64);
    let n1269: ZW = zw_mix2(n1267, zw_splat(n1171), 311u64);
    let n1270: ZW = zw_mix1(n1268, zw_splat(n1032), 312u64);
    let n1271: ZW = zw_mix2(n1269, zw_splat(n1032), 312u64);
    let n1272: ZW = zw_mix1(n1270, zw_splat(n1035), 320u64);
    let n1273: ZW = zw_mix2(n1271, zw_splat(n1035), 320u64);
    let n1274: ZW = zw_mix1(n1272, zw_splat(n1155), 321u64);
    let n1275: ZW = zw_mix2(n1273, zw_splat(n1155), 321u64);
    let n1276: ZW = zw_mix1(n1262, zw_splat(n1135), 309u64);
    let n1277: ZW = zw_mix2(n1263, zw_splat(n1135), 309u64);
    let n1278: ZW = zw_mix1(n1276, zw_splat(n1148), 310u64);
    let n1279: ZW = zw_mix2(n1277, zw_splat(n1148), 310u64);
    let n1280: ZW = zw_mix1(n1278, zw_splat(n1171), 311u64);
    let n1281: ZW = zw_mix2(n1279, zw_splat(n1171), 311u64);
    let n1282: ZW = zw_mix1(n1280, zw_splat(n1065), 312u64);
    let n1283: ZW = zw_mix2(n1281, zw_splat(n1065), 312u64);
    let n1284: ZW = zw_mix1(n1282, zw_splat(n1188), 320u64);
    let n1285: ZW = zw_mix2(n1283, zw_splat(n1188), 320u64);
    let n1286: ZW = zw_mix1(n1284, zw_splat(n1188), 321u64);
    let n1287: ZW = zw_mix2(n1285, zw_splat(n1188), 321u64);
    let n1288: ZW = zw_mix1(n1276, zw_splat(n1113), 310u64);
    let n1289: ZW = zw_mix2(n1277, zw_splat(n1113), 310u64);
    let n1290: ZW = zw_mix1(n1288, zw_splat(n1171), 311u64);
    let n1291: ZW = zw_mix2(n1289, zw_splat(n1171), 311u64);
    let n1292: ZW = zw_mix1(n1290, zw_splat(n1032), 312u64);
    let n1293: ZW = zw_mix2(n1291, zw_splat(n1032), 312u64);
    let n1294: ZW = zw_mix1(n1292, zw_splat(n1199), 320u64);
    let n1295: ZW = zw_mix2(n1293, zw_splat(n1199), 320u64);
    let n1296: ZW = zw_mix1(n1294, zw_splat(n1188), 321u64);
    let n1297: ZW = zw_mix2(n1295, zw_splat(n1188), 321u64);
    let n1298: ZW = zw_mix1(n1266, zw_splat(n1113), 311u64);
    let n1299: ZW = zw_mix2(n1267, zw_splat(n1113), 311u64);
    let n1300: ZW = zw_mix1(n1298, zw_splat(n1032), 312u64);
    let n1301: ZW = zw_mix2(n1299, zw_splat(n1032), 312u64);
    let n1302: ZW = zw_mix1(n1300, zw_splat(n1035), 320u64);
    let n1303: ZW = zw_mix2(n1301, zw_splat(n1035), 320u64);
    let n1304: ZW = zw_mix1(n1302, zw_splat(n1160), 321u64);
    let n1305: ZW = zw_mix2(n1303, zw_splat(n1160), 321u64);
    let n1306: ZW = zw_mix1(n1278, zw_splat(n1113), 311u64);
    let n1307: ZW = zw_mix2(n1279, zw_splat(n1113), 311u64);
    let n1308: ZW = zw_mix1(n1306, zw_splat(n1065), 312u64);
    let n1309: ZW = zw_mix2(n1307, zw_splat(n1065), 312u64);
    let n1310: ZW = zw_mix1(n1308, zw_splat(n1188), 320u64);
    let n1311: ZW = zw_mix2(n1309, zw_splat(n1188), 320u64);
    let n1312: ZW = zw_mix1(n1310, zw_splat(n1199), 321u64);
    let n1313: ZW = zw_mix2(n1311, zw_splat(n1199), 321u64);
    let n1314: ZW = zw_mix1(n1288, zw_splat(n1113), 311u64);
    let n1315: ZW = zw_mix2(n1289, zw_splat(n1113), 311u64);
    let n1316: ZW = zw_mix1(n1314, zw_splat(n1032), 312u64);
    let n1317: ZW = zw_mix2(n1315, zw_splat(n1032), 312u64);
    let n1318: ZW = zw_mix1(n1316, zw_splat(n1199), 320u64);
    let n1319: ZW = zw_mix2(n1317, zw_splat(n1199), 320u64);
    let n1320: ZW = zw_mix1(n1318, zw_splat(n1199), 321u64);
    let n1321: ZW = zw_mix2(n1319, zw_splat(n1199), 321u64);
    let n1322: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1029, 20u64);
    let n1323: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1029, 20u64);
    let n1324: ZW = zw_mix1(n1322, zw_splat(n1032), 41u64);
    let n1325: ZW = zw_mix2(n1323, zw_splat(n1032), 41u64);
    let n1326: u64 = mix64(11400714819323198485u64 ^ mix64(n1113 ^ 20u64));
    let n1327: u64 = 11562461410679940143u64.wrapping_add(mix64(n1113.wrapping_mul((20u64 << 1) | 1)));
    let n1328: u64 = mix64(n1326 ^ mix64(n1065 ^ 41u64));
    let n1329: u64 = n1327.wrapping_add(mix64(n1065.wrapping_mul((41u64 << 1) | 1)));
    let n1330: ZW = zw_bits_b(n897);
    let n1331: ZW = zw_mix1(n1322, n1330, 38u64);
    let n1332: ZW = zw_mix2(n1323, n1330, 38u64);
    let n1333: ZW = zw_bits_n(n896);
    let n1334: ZW = zw_mix1(n1331, n1333, 39u64);
    let n1335: ZW = zw_mix2(n1332, n1333, 39u64);
    let n1336: ZW = zw_bits_b(n905);
    let n1337: ZW = zw_mix1(n1322, n1336, 38u64);
    let n1338: ZW = zw_mix2(n1323, n1336, 38u64);
    let n1339: ZW = zw_bits_n(n904);
    let n1340: ZW = zw_mix1(n1337, n1339, 39u64);
    let n1341: ZW = zw_mix2(n1338, n1339, 39u64);
    let n1342: ZW = zw_bits_b(n913);
    let n1343: ZW = zw_mix1(n1322, n1342, 38u64);
    let n1344: ZW = zw_mix2(n1323, n1342, 38u64);
    let n1345: ZW = zw_bits_n(n912);
    let n1346: ZW = zw_mix1(n1343, n1345, 39u64);
    let n1347: ZW = zw_mix2(n1344, n1345, 39u64);
    let n1348: ZW = zw_bits_b(n921);
    let n1349: ZW = zw_mix1(n1322, n1348, 38u64);
    let n1350: ZW = zw_mix2(n1323, n1348, 38u64);
    let n1351: ZW = zw_bits_n(n920);
    let n1352: ZW = zw_mix1(n1349, n1351, 39u64);
    let n1353: ZW = zw_mix2(n1350, n1351, 39u64);
    let n1354: ZW = zw_bits_b(n929);
    let n1355: ZW = zw_mix1(n1322, n1354, 38u64);
    let n1356: ZW = zw_mix2(n1323, n1354, 38u64);
    let n1357: ZW = zw_bits_n(n928);
    let n1358: ZW = zw_mix1(n1355, n1357, 39u64);
    let n1359: ZW = zw_mix2(n1356, n1357, 39u64);
    let n1360: ZW = zw_bits_b(n937);
    let n1361: ZW = zw_mix1(n1322, n1360, 38u64);
    let n1362: ZW = zw_mix2(n1323, n1360, 38u64);
    let n1363: ZW = zw_bits_n(n936);
    let n1364: ZW = zw_mix1(n1361, n1363, 39u64);
    let n1365: ZW = zw_mix2(n1362, n1363, 39u64);
    let n1366: ZW = zw_mix1(zw_splat(n1326), n1330, 38u64);
    let n1367: ZW = zw_mix2(zw_splat(n1327), n1330, 38u64);
    let n1368: ZW = zw_mix1(n1366, n1333, 39u64);
    let n1369: ZW = zw_mix2(n1367, n1333, 39u64);
    let n1370: ZW = zw_mix1(zw_splat(n1326), n1336, 38u64);
    let n1371: ZW = zw_mix2(zw_splat(n1327), n1336, 38u64);
    let n1372: ZW = zw_mix1(n1370, n1339, 39u64);
    let n1373: ZW = zw_mix2(n1371, n1339, 39u64);
    let n1374: ZW = zw_mix1(zw_splat(n1326), n1342, 38u64);
    let n1375: ZW = zw_mix2(zw_splat(n1327), n1342, 38u64);
    let n1376: ZW = zw_mix1(n1374, n1345, 39u64);
    let n1377: ZW = zw_mix2(n1375, n1345, 39u64);
    let n1378: ZW = zw_mix1(zw_splat(n1326), n1348, 38u64);
    let n1379: ZW = zw_mix2(zw_splat(n1327), n1348, 38u64);
    let n1380: ZW = zw_mix1(n1378, n1351, 39u64);
    let n1381: ZW = zw_mix2(n1379, n1351, 39u64);
    let n1382: ZW = zw_mix1(zw_splat(n1326), n1354, 38u64);
    let n1383: ZW = zw_mix2(zw_splat(n1327), n1354, 38u64);
    let n1384: ZW = zw_mix1(n1382, n1357, 39u64);
    let n1385: ZW = zw_mix2(n1383, n1357, 39u64);
    let n1386: ZW = zw_mix1(zw_splat(n1326), n1360, 38u64);
    let n1387: ZW = zw_mix2(zw_splat(n1327), n1360, 38u64);
    let n1388: ZW = zw_mix1(n1386, n1363, 39u64);
    let n1389: ZW = zw_mix2(n1387, n1363, 39u64);
    let n1390: ZW = zw_bits_n(n949);
    let n1391: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1390, 20u64);
    let n1392: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1390, 20u64);
    let n1393: ZW = zw_mix1(n1391, n1023, 39u64);
    let n1394: ZW = zw_mix2(n1392, n1023, 39u64);
    let n1395: ZW = zw_bits_n(n1012);
    let n1396: ZW = zw_mix1(n1393, n1395, 252u64);
    let n1397: ZW = zw_mix2(n1394, n1395, 252u64);
    let n1398: ZW = zw_bits_n(n979);
    let n1399: ZW = zw_mix1(n1396, n1398, 263u64);
    let n1400: ZW = zw_mix2(n1397, n1398, 263u64);
    let n1401: ZW = zw_bits_n(n1013);
    let n1402: ZW = zw_mix1(n1399, n1401, 264u64);
    let n1403: ZW = zw_mix2(n1400, n1401, 264u64);
    let n1404: ZW = zw_bits_n(n1014);
    let n1405: ZW = zw_mix1(n1402, n1404, 268u64);
    let n1406: ZW = zw_mix2(n1403, n1404, 268u64);
    let n1407: ZW = zw_bits_n(n980);
    let n1408: ZW = zw_mix1(n1405, n1407, 294u64);
    let n1409: ZW = zw_mix2(n1406, n1407, 294u64);
    let n1410: ZW = zw_bits_n(n981);
    let n1411: ZW = zw_mix1(n1408, n1410, 295u64);
    let n1412: ZW = zw_mix2(n1409, n1410, 295u64);
    let n1413: ZW = zw_bits_n(n1015);
    let n1414: ZW = zw_mix1(n1411, n1413, 309u64);
    let n1415: ZW = zw_mix2(n1412, n1413, 309u64);
    let n1416: ZW = zw_bits_n(n1016);
    let n1417: ZW = zw_mix1(n1414, n1416, 311u64);
    let n1418: ZW = zw_mix2(n1415, n1416, 311u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v0_b0: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b0: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n426);
    let ok_v1_b1: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v1_b1: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v1_b1: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n465);
    let ok_v2_b2: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v2_b2: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v2_b2: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n501);
    let ok_v16_b3: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v16_b3: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v16_b3: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n546);
    let ok_v17_b4: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v17_b4: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v17_b4: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n591);
    let ok_v18_b5: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v18_b5: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v18_b5: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n636);
    let ok_v32_b6: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v32_b6: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v32_b6: u16 = ALL & zb_holds(n423) & zb_holds(n426);
    let ok_v33_b7: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v33_b7: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v33_b7: u16 = ALL & zb_holds(n423) & zb_holds(n465);
    let ok_v34_b8: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v34_b8: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v34_b8: u16 = ALL & zb_holds(n423) & zb_holds(n501);
    let ok_v36_b9: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v36_b9: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v36_b9: u16 = ALL & zb_holds(n423) & zb_holds(n426);
    let ok_v37_b10: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v37_b10: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v37_b10: u16 = ALL & zb_holds(n423) & zb_holds(n465);
    let ok_v38_b11: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v38_b11: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v38_b11: u16 = ALL & zb_holds(n423) & zb_holds(n501);
    let ok_v40_b12: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v40_b12: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v40_b12: u16 = ALL & zb_holds(n423) & zb_holds(n426);
    let ok_v41_b13: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v41_b13: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v41_b13: u16 = ALL & zb_holds(n423) & zb_holds(n465);
    let ok_v42_b14: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v42_b14: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v42_b14: u16 = ALL & zb_holds(n423) & zb_holds(n501);
    let ok_v48_b15: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v48_b15: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v48_b15: u16 = ALL & zb_holds(n423) & zb_holds(n546);
    let ok_v49_b16: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v49_b16: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v49_b16: u16 = ALL & zb_holds(n423) & zb_holds(n591);
    let ok_v50_b17: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v50_b17: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v50_b17: u16 = ALL & zb_holds(n423) & zb_holds(n636);
    let ok_v52_b18: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v52_b18: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v52_b18: u16 = ALL & zb_holds(n423) & zb_holds(n546);
    let ok_v53_b19: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v53_b19: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v53_b19: u16 = ALL & zb_holds(n423) & zb_holds(n591);
    let ok_v54_b20: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v54_b20: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v54_b20: u16 = ALL & zb_holds(n423) & zb_holds(n636);
    let ok_v56_b21: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v56_b21: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v56_b21: u16 = ALL & zb_holds(n423) & zb_holds(n546);
    let ok_v57_b22: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v57_b22: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v57_b22: u16 = ALL & zb_holds(n423) & zb_holds(n591);
    let ok_v58_b23: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n351);
    let bd_v58_b23: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v58_b23: u16 = ALL & zb_holds(n423) & zb_holds(n636);
    let ok_v0_b24: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v0_b24: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b24: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n696);
    let ok_v1_b25: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v1_b25: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v1_b25: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n726);
    let ok_v2_b26: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v2_b26: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v2_b26: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n755);
    let ok_v16_b27: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v16_b27: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v16_b27: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n800);
    let ok_v17_b28: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v17_b28: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v17_b28: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n845);
    let ok_v18_b29: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v18_b29: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v18_b29: u16 = ALL & zb_holds(n122) & zb_holds(n423) & zb_holds(n890);
    let ok_v32_b30: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v32_b30: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v32_b30: u16 = ALL & zb_holds(n423) & zb_holds(n696);
    let ok_v33_b31: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v33_b31: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v33_b31: u16 = ALL & zb_holds(n423) & zb_holds(n726);
    let ok_v34_b32: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v34_b32: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v34_b32: u16 = ALL & zb_holds(n423) & zb_holds(n755);
    let ok_v48_b33: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v48_b33: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v48_b33: u16 = ALL & zb_holds(n423) & zb_holds(n800);
    let ok_v49_b34: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v49_b34: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v49_b34: u16 = ALL & zb_holds(n423) & zb_holds(n845);
    let ok_v50_b35: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n669);
    let bd_v50_b35: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v50_b35: u16 = ALL & zb_holds(n423) & zb_holds(n890);
    let ok_v0_b36: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n899);
    let bd_v0_b36: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b36: u16 = ALL & zb_holds(n122) & zb_holds(n898);
    let ok_v1_b37: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n907);
    let bd_v1_b37: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v1_b37: u16 = ALL & zb_holds(n122) & zb_holds(n906);
    let ok_v2_b38: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n915);
    let bd_v2_b38: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v2_b38: u16 = ALL & zb_holds(n122) & zb_holds(n914);
    let ok_v16_b39: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n923);
    let bd_v16_b39: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v16_b39: u16 = ALL & zb_holds(n122) & zb_holds(n922);
    let ok_v17_b40: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n931);
    let bd_v17_b40: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v17_b40: u16 = ALL & zb_holds(n122) & zb_holds(n930);
    let ok_v18_b41: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n939);
    let bd_v18_b41: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v18_b41: u16 = ALL & zb_holds(n122) & zb_holds(n938);
    let ok_v32_b42: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n899);
    let bd_v32_b42: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v32_b42: u16 = ALL & zb_holds(n898);
    let ok_v33_b43: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n907);
    let bd_v33_b43: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v33_b43: u16 = ALL & zb_holds(n906);
    let ok_v34_b44: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n915);
    let bd_v34_b44: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v34_b44: u16 = ALL & zb_holds(n914);
    let ok_v48_b45: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n923);
    let bd_v48_b45: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v48_b45: u16 = ALL & zb_holds(n922);
    let ok_v49_b46: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n931);
    let bd_v49_b46: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v49_b46: u16 = ALL & zb_holds(n930);
    let ok_v50_b47: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70) & zb_holds(n939);
    let bd_v50_b47: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v50_b47: u16 = ALL & zb_holds(n938);
    let ok_v0_b48: u16 = ALL & zb_holds(n101) & zb_holds(n100) & zb_holds(n99) & zb_holds(n98) & zb_holds(n97) & zb_holds(n96) & zb_holds(n94) & zb_holds(n93) & zb_holds(n92) & zb_holds(n87) & zb_holds(n86) & zb_holds(r_c251) & zb_holds(n85) & zb_holds(n84) & zb_holds(n83) & zb_holds(n82) & zb_holds(n81) & zb_holds(r_c243) & zb_holds(n80) & zb_holds(n79) & zb_holds(n76) & zb_holds(n75) & zb_holds(r_c234) & zb_holds(n74) & zb_holds(n73) & zb_holds(n72) & zb_holds(n71) & zb_holds(n69) & zb_holds(n70);
    let bd_v0_b48: bool = !n91 || !n90 || !n89 || !n88 || !n78 || !n77;
    let live_v0_b48: u16 = ALL & zb_holds(n1020);
    let sh0 = KShared0 {
        c39: r_c39,
        c273: n168,
    };
    let sh1 = KShared1 {
    };
    let sh2 = KShared2 {
    };
    let sh3 = KShared3 {
        c39: r_c39,
        c20: n949,
        c294: n980,
        c295: n981,
        c252: n1012,
        c309: n1015,
        c311: n1016,
        c263: n979,
        c264: n1013,
        c268: n1014,
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
    let mut take_3_0: u16 = 0;
    // 49 distinct button assignments; per outcome they fall
    // into [24, 2, 12, 1] groups that write identical values.
    declined |= live_v0_b0 & (if bd_v0_b0 { ALL } else { !ok_v0_b0 });
    take_0_0 |= live_v0_b0 & ok_v0_b0 & (if bd_v0_b0 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c308: zn_splat(P8::from_raw(0i32)),
        c309: zn_splat(P8::from_raw(0i32)),
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n382,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: n399,
        h1: n1063, h2: n1064,
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
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(true),
        c258: n382,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: n432,
        c321: n399,
        h1: n1071, h2: n1072,
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
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n382,
        c265: zb_splat(false),
        c266: zb_splat(false),
        c320: n468,
        c321: n399,
        h1: n1076, h2: n1077,
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
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n419,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n420,
        c321: n421,
        h1: n1099, h2: n1100,
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
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(true),
        c258: n419,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n462,
        c321: n421,
        h1: n1106, h2: n1107,
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
        c253: zn_splat(P8::from_raw(0i32)),
        c310: zn_splat(P8::from_raw(0i32)),
        c311: zn_splat(P8::from_raw(0i32)),
        c255: zn_splat(P8::from_raw(0i32)),
        c256: zn_splat(P8::from_raw(65536i32)),
        c312: zb_splat(false),
        c258: n419,
        c265: zb_splat(false),
        c266: zb_splat(true),
        c320: n498,
        c321: n421,
        h1: n1111, h2: n1112,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(65536i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n1146, h2: n1147,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n1158, h2: n1159,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n1163, h2: n1164,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(-327680i32)),
        h1: n1178, h2: n1179,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n1191, h2: n1192,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n1202, h2: n1203,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(327680i32)),
        h1: n1210, h2: n1211,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n1218, h2: n1219,
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
        c258: n382,
        c265: zb_splat(true),
        c266: zb_splat(false),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n1226, h2: n1227,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(65536i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n1246, h2: n1247,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n1256, h2: n1257,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(327680i32)),
        c321: zn_splat(P8::from_raw(0i32)),
        h1: n1260, h2: n1261,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(-327680i32)),
        h1: n1274, h2: n1275,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n1286, h2: n1287,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(-231700i32)),
        h1: n1296, h2: n1297,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(0i32)),
        c321: zn_splat(P8::from_raw(327680i32)),
        h1: n1304, h2: n1305,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(-231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n1312, h2: n1313,
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
        c258: n419,
        c265: zb_splat(true),
        c266: zb_splat(true),
        c320: zn_splat(P8::from_raw(231700i32)),
        c321: zn_splat(P8::from_raw(231700i32)),
        h1: n1320, h2: n1321,
    };
    // body 23: buttons 0x3a, forks 0x0
    sink.o0(58, take_0_23, &sh0, &o0);
    declined |= live_v0_b24 & (if bd_v0_b24 { ALL } else { !ok_v0_b24 });
    take_1_0 |= live_v0_b24 & ok_v0_b24 & (if bd_v0_b24 { 0 } else { ALL });
    declined |= live_v1_b25 & (if bd_v1_b25 { ALL } else { !ok_v1_b25 });
    take_1_0 |= live_v1_b25 & ok_v1_b25 & (if bd_v1_b25 { 0 } else { ALL });
    declined |= live_v2_b26 & (if bd_v2_b26 { ALL } else { !ok_v2_b26 });
    take_1_0 |= live_v2_b26 & ok_v2_b26 & (if bd_v2_b26 { 0 } else { ALL });
    declined |= live_v16_b27 & (if bd_v16_b27 { ALL } else { !ok_v16_b27 });
    take_1_0 |= live_v16_b27 & ok_v16_b27 & (if bd_v16_b27 { 0 } else { ALL });
    declined |= live_v17_b28 & (if bd_v17_b28 { ALL } else { !ok_v17_b28 });
    take_1_0 |= live_v17_b28 & ok_v17_b28 & (if bd_v17_b28 { 0 } else { ALL });
    declined |= live_v18_b29 & (if bd_v18_b29 { ALL } else { !ok_v18_b29 });
    take_1_0 |= live_v18_b29 & ok_v18_b29 & (if bd_v18_b29 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: r_c20,
        c41: zb_splat(false),
        h1: n1324, h2: n1325,
    };
    // body 29: buttons 0x12, forks 0x0
    sink.o1(18, take_1_0, &sh1, &o1);
    declined |= live_v32_b30 & (if bd_v32_b30 { ALL } else { !ok_v32_b30 });
    take_1_1 |= live_v32_b30 & ok_v32_b30 & (if bd_v32_b30 { 0 } else { ALL });
    declined |= live_v33_b31 & (if bd_v33_b31 { ALL } else { !ok_v33_b31 });
    take_1_1 |= live_v33_b31 & ok_v33_b31 & (if bd_v33_b31 { 0 } else { ALL });
    declined |= live_v34_b32 & (if bd_v34_b32 { ALL } else { !ok_v34_b32 });
    take_1_1 |= live_v34_b32 & ok_v34_b32 & (if bd_v34_b32 { 0 } else { ALL });
    declined |= live_v48_b33 & (if bd_v48_b33 { ALL } else { !ok_v48_b33 });
    take_1_1 |= live_v48_b33 & ok_v48_b33 & (if bd_v48_b33 { 0 } else { ALL });
    declined |= live_v49_b34 & (if bd_v49_b34 { ALL } else { !ok_v49_b34 });
    take_1_1 |= live_v49_b34 & ok_v49_b34 & (if bd_v49_b34 { 0 } else { ALL });
    declined |= live_v50_b35 & (if bd_v50_b35 { ALL } else { !ok_v50_b35 });
    take_1_1 |= live_v50_b35 & ok_v50_b35 & (if bd_v50_b35 { 0 } else { ALL });
    let o1 = KOut1 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        h1: zw_splat(n1328), h2: zw_splat(n1329),
    };
    // body 35: buttons 0x32, forks 0x0
    sink.o1(50, take_1_1, &sh1, &o1);
    declined |= live_v0_b36 & (if bd_v0_b36 { ALL } else { !ok_v0_b36 });
    take_2_0 |= live_v0_b36 & ok_v0_b36 & (if bd_v0_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n896,
        c20: r_c20,
        c38: n897,
        h1: n1334, h2: n1335,
    };
    // body 36: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b37 & (if bd_v1_b37 { ALL } else { !ok_v1_b37 });
    take_2_1 |= live_v1_b37 & ok_v1_b37 & (if bd_v1_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n904,
        c20: r_c20,
        c38: n905,
        h1: n1340, h2: n1341,
    };
    // body 37: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b38 & (if bd_v2_b38 { ALL } else { !ok_v2_b38 });
    take_2_2 |= live_v2_b38 & ok_v2_b38 & (if bd_v2_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n912,
        c20: r_c20,
        c38: n913,
        h1: n1346, h2: n1347,
    };
    // body 38: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b39 & (if bd_v16_b39 { ALL } else { !ok_v16_b39 });
    take_2_3 |= live_v16_b39 & ok_v16_b39 & (if bd_v16_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n920,
        c20: r_c20,
        c38: n921,
        h1: n1352, h2: n1353,
    };
    // body 39: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b40 & (if bd_v17_b40 { ALL } else { !ok_v17_b40 });
    take_2_4 |= live_v17_b40 & ok_v17_b40 & (if bd_v17_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n928,
        c20: r_c20,
        c38: n929,
        h1: n1358, h2: n1359,
    };
    // body 40: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b41 & (if bd_v18_b41 { ALL } else { !ok_v18_b41 });
    take_2_5 |= live_v18_b41 & ok_v18_b41 & (if bd_v18_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n936,
        c20: r_c20,
        c38: n937,
        h1: n1364, h2: n1365,
    };
    // body 41: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b42 & (if bd_v32_b42 { ALL } else { !ok_v32_b42 });
    take_2_6 |= live_v32_b42 & ok_v32_b42 & (if bd_v32_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n896,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n897,
        h1: n1368, h2: n1369,
    };
    // body 42: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b43 & (if bd_v33_b43 { ALL } else { !ok_v33_b43 });
    take_2_7 |= live_v33_b43 & ok_v33_b43 & (if bd_v33_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n904,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n905,
        h1: n1372, h2: n1373,
    };
    // body 43: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b44 & (if bd_v34_b44 { ALL } else { !ok_v34_b44 });
    take_2_8 |= live_v34_b44 & ok_v34_b44 & (if bd_v34_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n912,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n913,
        h1: n1376, h2: n1377,
    };
    // body 44: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_2_9 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n920,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n921,
        h1: n1380, h2: n1381,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o2(48, take_2_9, &sh2, &o2);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_2_10 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n928,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n929,
        h1: n1384, h2: n1385,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o2(49, take_2_10, &sh2, &o2);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_2_11 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c39: n936,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n937,
        h1: n1388, h2: n1389,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o2(50, take_2_11, &sh2, &o2);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_3_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n1417, h2: n1418,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
