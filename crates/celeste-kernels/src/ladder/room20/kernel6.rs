// GENERATED from a TRACED frame (shape 6). Do not edit.
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
pub const SHAPE: u64 = 1100049596121686425;

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
    ("objects[2].hitbox.h", "num"),
    ("objects[2].hitbox.w", "num"),
    ("objects[2].hitbox.x", "num"),
    ("objects[2].hitbox.y", "num"),
];

/// Block-uniform inputs, in `UNI_SLOTS` order.
pub struct Uni {
    pub c332: P8,
    pub c333: P8,
    pub c334: P8,
    pub c335: P8,
    pub c344: P8,
    pub c345: P8,
    pub c346: P8,
    pub c347: P8,
    pub c354: P8,
    pub c355: P8,
    pub c356: P8,
    pub c357: P8,
}

/// (path, kind) - resolved against a block at bind time.
pub const ROW_SLOTS: &[(&str, &str)] = &[
    ("deaths", "num"),
    ("delay_restart", "num"),
    ("frames", "num"),
    ("freeze", "num"),
    ("got_fruit[#3]", "bool"),
    ("has_dashed", "bool"),
    ("has_key", "bool"),
    ("max_djump", "num"),
    ("minutes", "num"),
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
    ("objects[1].collideable", "bool"),
    ("objects[1].flip.x", "bool"),
    ("objects[1].flip.y", "bool"),
    ("objects[1].hide_for", "num"),
    ("objects[1].hide_in", "num"),
    ("objects[1].rem.x", "num"),
    ("objects[1].rem.y", "num"),
    ("objects[1].solids", "bool"),
    ("objects[1].spd.x", "num"),
    ("objects[1].spd.y", "num"),
    ("objects[1].spr", "num"),
    ("objects[1].x", "num"),
    ("objects[1].y", "num"),
    ("objects[2].collideable", "bool"),
    ("objects[2].flip.x", "bool"),
    ("objects[2].flip.y", "bool"),
    ("objects[2].hide_for", "num"),
    ("objects[2].hide_in", "num"),
    ("objects[2].rem.x", "num"),
    ("objects[2].rem.y", "num"),
    ("objects[2].solids", "bool"),
    ("objects[2].spd.x", "num"),
    ("objects[2].spd.y", "num"),
    ("objects[2].spr", "num"),
    ("objects[2].x", "num"),
    ("objects[2].y", "num"),
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
    pub c175: u16,
    pub c41: u16,
    pub c42: u16,
    pub c88: ZN,
    pub c86: ZN,
    pub c240: u16,
    pub c241: ZN,
    pub c330: u16,
    pub c331: u16,
    pub c336: ZN,
    pub c337: ZN,
    pub c250: u16,
    pub c338: ZN,
    pub c339: ZN,
    pub c252: ZN,
    pub c253: ZN,
    pub c340: ZN,
    pub c341: ZN,
    pub c256: ZN,
    pub c257: ZN,
    pub c260: u16,
    pub c342: u16,
    pub c343: u16,
    pub c262: ZN,
    pub c263: ZN,
    pub c348: ZN,
    pub c349: ZN,
    pub c271: u16,
    pub c350: ZN,
    pub c351: ZN,
    pub c273: ZN,
    pub c275: ZN,
    pub c276: ZN,
    pub c279: u16,
    pub c352: u16,
    pub c353: u16,
    pub c281: ZN,
    pub c282: ZN,
    pub c358: ZN,
    pub c359: ZN,
    pub c290: u16,
    pub c360: ZN,
    pub c361: ZN,
    pub c292: ZN,
    pub c294: ZN,
    pub c295: ZN,
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
    pub c175: u32,
    pub c41: u32,
    pub c42: u32,
    pub c88: u32,
    pub c86: u32,
    pub c240: u32,
    pub c241: u32,
    pub c330: u32,
    pub c331: u32,
    pub c336: u32,
    pub c337: u32,
    pub c250: u32,
    pub c338: u32,
    pub c339: u32,
    pub c252: u32,
    pub c253: u32,
    pub c340: u32,
    pub c341: u32,
    pub c256: u32,
    pub c257: u32,
    pub c260: u32,
    pub c342: u32,
    pub c343: u32,
    pub c262: u32,
    pub c263: u32,
    pub c348: u32,
    pub c349: u32,
    pub c271: u32,
    pub c350: u32,
    pub c351: u32,
    pub c273: u32,
    pub c275: u32,
    pub c276: u32,
    pub c279: u32,
    pub c352: u32,
    pub c353: u32,
    pub c281: u32,
    pub c282: u32,
    pub c358: u32,
    pub c359: u32,
    pub c290: u32,
    pub c360: u32,
    pub c361: u32,
    pub c292: u32,
    pub c294: u32,
    pub c295: u32,
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
        c332: match &b.cols[cell("objects[0].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c333: match &b.cols[cell("objects[0].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c334: match &b.cols[cell("objects[0].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c335: match &b.cols[cell("objects[0].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c344: match &b.cols[cell("objects[1].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c345: match &b.cols[cell("objects[1].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c346: match &b.cols[cell("objects[1].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c347: match &b.cols[cell("objects[1].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c354: match &b.cols[cell("objects[2].hitbox.h")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c355: match &b.cols[cell("objects[2].hitbox.w")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c356: match &b.cols[cell("objects[2].hitbox.x")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
        c357: match &b.cols[cell("objects[2].hitbox.y")? as usize] { Col::U(AV::Num(n)) => *n, _ => return None },
    };
    let s = RowSlots {
        c87: cell("deaths")?,
        c39: cell("delay_restart")?,
        c84: cell("frames")?,
        c20: cell("freeze")?,
        c175: cell("got_fruit[#3]")?,
        c41: cell("has_dashed")?,
        c42: cell("has_key")?,
        c88: cell("max_djump")?,
        c86: cell("minutes")?,
        c240: cell("objects[0].collideable")?,
        c241: cell("objects[0].delay")?,
        c330: cell("objects[0].flip.x")?,
        c331: cell("objects[0].flip.y")?,
        c336: cell("objects[0].rem.x")?,
        c337: cell("objects[0].rem.y")?,
        c250: cell("objects[0].solids")?,
        c338: cell("objects[0].spd.x")?,
        c339: cell("objects[0].spd.y")?,
        c252: cell("objects[0].spr")?,
        c253: cell("objects[0].state")?,
        c340: cell("objects[0].target.x")?,
        c341: cell("objects[0].target.y")?,
        c256: cell("objects[0].x")?,
        c257: cell("objects[0].y")?,
        c260: cell("objects[1].collideable")?,
        c342: cell("objects[1].flip.x")?,
        c343: cell("objects[1].flip.y")?,
        c262: cell("objects[1].hide_for")?,
        c263: cell("objects[1].hide_in")?,
        c348: cell("objects[1].rem.x")?,
        c349: cell("objects[1].rem.y")?,
        c271: cell("objects[1].solids")?,
        c350: cell("objects[1].spd.x")?,
        c351: cell("objects[1].spd.y")?,
        c273: cell("objects[1].spr")?,
        c275: cell("objects[1].x")?,
        c276: cell("objects[1].y")?,
        c279: cell("objects[2].collideable")?,
        c352: cell("objects[2].flip.x")?,
        c353: cell("objects[2].flip.y")?,
        c281: cell("objects[2].hide_for")?,
        c282: cell("objects[2].hide_in")?,
        c358: cell("objects[2].rem.x")?,
        c359: cell("objects[2].rem.y")?,
        c290: cell("objects[2].solids")?,
        c360: cell("objects[2].spd.x")?,
        c361: cell("objects[2].spd.y")?,
        c292: cell("objects[2].spr")?,
        c294: cell("objects[2].x")?,
        c295: cell("objects[2].y")?,
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
        c175: match &b.cols[s.c175 as usize] {
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
        c240: match &b.cols[s.c240 as usize] {
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
        c241: match &b.cols[s.c241 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c330: match &b.cols[s.c330 as usize] {
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
        c331: match &b.cols[s.c331 as usize] {
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
        c336: match &b.cols[s.c336 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c337: match &b.cols[s.c337 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c250: match &b.cols[s.c250 as usize] {
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
        c338: match &b.cols[s.c338 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c339: match &b.cols[s.c339 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c252: match &b.cols[s.c252 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c253: match &b.cols[s.c253 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c340: match &b.cols[s.c340 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c341: match &b.cols[s.c341 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c256: match &b.cols[s.c256 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c257: match &b.cols[s.c257 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c260: match &b.cols[s.c260 as usize] {
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
        c342: match &b.cols[s.c342 as usize] {
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
        c343: match &b.cols[s.c343 as usize] {
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
        c262: match &b.cols[s.c262 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c263: match &b.cols[s.c263 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c348: match &b.cols[s.c348 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c349: match &b.cols[s.c349 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c271: match &b.cols[s.c271 as usize] {
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
        c350: match &b.cols[s.c350 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c351: match &b.cols[s.c351 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c273: match &b.cols[s.c273 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c275: match &b.cols[s.c275 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c276: match &b.cols[s.c276 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c279: match &b.cols[s.c279 as usize] {
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
        c352: match &b.cols[s.c352 as usize] {
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
        c353: match &b.cols[s.c353 as usize] {
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
        c281: match &b.cols[s.c281 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c282: match &b.cols[s.c282 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c358: match &b.cols[s.c358 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c359: match &b.cols[s.c359 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c290: match &b.cols[s.c290 as usize] {
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
        c360: match &b.cols[s.c360 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c361: match &b.cols[s.c361 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c292: match &b.cols[s.c292 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c294: match &b.cols[s.c294 as usize] {
            Col::N(v) => ZN::from_array(core::array::from_fn(|i| v[at(i)])),
            Col::U(AV::Num(n)) => zn_splat(*n),
            _ => return None,
        },
        c295: match &b.cols[s.c295 as usize] {
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
    (180, "balloon.tile"),
    (207, "big_chest.tile"),
    (199, "chest.if_not_fruit"),
    (201, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (193, "fake_wall.if_not_fruit"),
    (194, "fake_wall.tile"),
    (183, "fall_floor.tile"),
    (189, "fly_fruit.if_not_fruit"),
    (191, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (185, "fruit.if_not_fruit"),
    (187, "fruit.tile"),
    (175, "got_fruit[#3]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (196, "key.if_not_fruit"),
    (197, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (240, "objects[0].collideable"),
    (336, "objects[0].flip.x"),
    (337, "objects[0].flip.y"),
    (242, "objects[0].hide_for"),
    (243, "objects[0].hide_in"),
    (338, "objects[0].hitbox.h"),
    (339, "objects[0].hitbox.w"),
    (340, "objects[0].hitbox.x"),
    (341, "objects[0].hitbox.y"),
    (342, "objects[0].rem.x"),
    (343, "objects[0].rem.y"),
    (251, "objects[0].solids"),
    (344, "objects[0].spd.x"),
    (345, "objects[0].spd.y"),
    (253, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (255, "objects[0].x"),
    (256, "objects[0].y"),
    (259, "objects[1].collideable"),
    (346, "objects[1].flip.x"),
    (347, "objects[1].flip.y"),
    (261, "objects[1].hide_for"),
    (262, "objects[1].hide_in"),
    (348, "objects[1].hitbox.h"),
    (349, "objects[1].hitbox.w"),
    (350, "objects[1].hitbox.x"),
    (351, "objects[1].hitbox.y"),
    (352, "objects[1].rem.x"),
    (353, "objects[1].rem.y"),
    (270, "objects[1].solids"),
    (354, "objects[1].spd.x"),
    (355, "objects[1].spd.y"),
    (272, "objects[1].spr"),
    (274, "objects[1].x"),
    (275, "objects[1].y"),
    (278, "objects[2].collideable"),
    (356, "objects[2].dash_accel.x"),
    (357, "objects[2].dash_accel.y"),
    (280, "objects[2].dash_effect_time"),
    (358, "objects[2].dash_target.x"),
    (359, "objects[2].dash_target.y"),
    (282, "objects[2].dash_time"),
    (283, "objects[2].djump"),
    (360, "objects[2].flip.x"),
    (361, "objects[2].flip.y"),
    (285, "objects[2].grace"),
    (362, "objects[2].hitbox.h"),
    (363, "objects[2].hitbox.w"),
    (364, "objects[2].hitbox.x"),
    (365, "objects[2].hitbox.y"),
    (292, "objects[2].p_dash"),
    (293, "objects[2].p_jump"),
    (366, "objects[2].rem.x"),
    (367, "objects[2].rem.y"),
    (295, "objects[2].solids"),
    (368, "objects[2].spd.x"),
    (369, "objects[2].spd.y"),
    (299, "objects[2].x"),
    (300, "objects[2].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
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
    SCell::Arr(&[173, 174, 175]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Obj(&[(5, 182), (8, 183), (6, 184)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (5, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(9, 199), (5, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(5, 203), (6, 204)]),
    SCell::Obj(&[(7, 205), (5, 206), (8, 207)]),
    SCell::Obj(&[(7, 208), (5, 209)]),
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
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 238), (20, 239), (11, 240), (14, 241), (45, 242), (42, 243), (15, 244), (19, 245), (18, 246), (22, 247), (23, 248), (24, 249), (4, 250), (12, 251), (3, 252), (13, 253), (0, 254), (1, 255), (2, 256)]),
    SCell::Obj(&[(21, 257), (20, 258), (11, 259), (14, 260), (45, 261), (42, 262), (15, 263), (19, 264), (18, 265), (22, 266), (23, 267), (24, 268), (4, 269), (12, 270), (3, 271), (13, 272), (0, 273), (1, 274), (2, 275)]),
    SCell::Obj(&[(21, 276), (20, 277), (11, 278), (28, 279), (33, 280), (27, 281), (26, 282), (30, 283), (14, 284), (29, 285), (15, 286), (19, 287), (18, 288), (22, 289), (23, 290), (24, 291), (32, 292), (31, 293), (4, 294), (12, 295), (3, 296), (13, 297), (0, 298), (1, 299), (2, 300)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 336), (2, 337)]),
    SCell::Obj(&[(17, 338), (16, 339), (1, 340), (2, 341)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 342), (2, 343)]),
    SCell::Obj(&[(1, 344), (2, 345)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 346), (2, 347)]),
    SCell::Obj(&[(17, 348), (16, 349), (1, 350), (2, 351)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 352), (2, 353)]),
    SCell::Obj(&[(1, 354), (2, 355)]),
    SCell::Clo(26, &[212]),
    SCell::Clo(25, &[212]),
    SCell::Obj(&[(1, 356), (2, 357)]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(1, 360), (2, 361)]),
    SCell::Obj(&[(17, 362), (16, 363), (1, 364), (2, 365)]),
    SCell::Clo(24, &[212]),
    SCell::Clo(23, &[212]),
    SCell::Clo(27, &[212]),
    SCell::Clo(28, &[212]),
    SCell::Clo(29, &[212]),
    SCell::Obj(&[(1, 366), (2, 367)]),
    SCell::Obj(&[(1, 368), (2, 369)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 210),
    (152, 211),
    (153, 212),
    (155, 213),
    (156, 214),
    (157, 215),
    (158, 216),
    (160, 217),
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
    (176, 218),
    (178, 219),
    (179, 220),
    (181, 221),
    (182, 222),
    (184, 223),
    (186, 224),
    (188, 225),
    (190, 226),
    (192, 227),
    (195, 228),
    (198, 229),
    (200, 230),
    (202, 231),
    (203, 232),
    (204, 233),
    (205, 234),
    (206, 235),
    (208, 236),
    (209, 237),
    (238, 301),
    (239, 302),
    (241, 303),
    (244, 304),
    (245, 305),
    (246, 306),
    (247, 307),
    (248, 308),
    (249, 309),
    (250, 310),
    (252, 311),
    (254, 116),
    (257, 312),
    (258, 313),
    (260, 314),
    (263, 315),
    (264, 316),
    (265, 317),
    (266, 318),
    (267, 319),
    (268, 320),
    (269, 321),
    (271, 322),
    (273, 116),
    (276, 323),
    (277, 324),
    (279, 325),
    (281, 326),
    (284, 327),
    (286, 328),
    (287, 329),
    (288, 330),
    (289, 331),
    (290, 332),
    (291, 333),
    (294, 334),
    (296, 335),
    (298, 93),
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
    pub c300: ZN,
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
    pub c356: ZN,
    pub c357: ZN,
    pub c280: ZN,
    pub c358: ZN,
    pub c359: ZN,
    pub c282: ZN,
    pub c283: ZN,
    pub c360: ZB,
    pub c285: ZN,
    pub c292: ZB,
    pub c293: ZB,
    pub c368: ZN,
    pub c369: ZN,
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
    (180, "balloon.tile"),
    (207, "big_chest.tile"),
    (199, "chest.if_not_fruit"),
    (201, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (193, "fake_wall.if_not_fruit"),
    (194, "fake_wall.tile"),
    (183, "fall_floor.tile"),
    (189, "fly_fruit.if_not_fruit"),
    (191, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (185, "fruit.if_not_fruit"),
    (187, "fruit.tile"),
    (175, "got_fruit[#3]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (196, "key.if_not_fruit"),
    (197, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (239, "objects[0].collideable"),
    (297, "objects[0].flip.x"),
    (298, "objects[0].flip.y"),
    (241, "objects[0].hide_for"),
    (242, "objects[0].hide_in"),
    (299, "objects[0].hitbox.h"),
    (300, "objects[0].hitbox.w"),
    (301, "objects[0].hitbox.x"),
    (302, "objects[0].hitbox.y"),
    (303, "objects[0].rem.x"),
    (304, "objects[0].rem.y"),
    (250, "objects[0].solids"),
    (305, "objects[0].spd.x"),
    (306, "objects[0].spd.y"),
    (252, "objects[0].spr"),
    (177, "objects[0].type.tile"),
    (254, "objects[0].x"),
    (255, "objects[0].y"),
    (258, "objects[1].collideable"),
    (307, "objects[1].flip.x"),
    (308, "objects[1].flip.y"),
    (260, "objects[1].hide_for"),
    (261, "objects[1].hide_in"),
    (309, "objects[1].hitbox.h"),
    (310, "objects[1].hitbox.w"),
    (311, "objects[1].hitbox.x"),
    (312, "objects[1].hitbox.y"),
    (313, "objects[1].rem.x"),
    (314, "objects[1].rem.y"),
    (269, "objects[1].solids"),
    (315, "objects[1].spd.x"),
    (316, "objects[1].spd.y"),
    (271, "objects[1].spr"),
    (273, "objects[1].x"),
    (274, "objects[1].y"),
    (43, "pause_player"),
    (159, "player_spawn.tile"),
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
    SCell::Arr(&[173, 174, 175]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Obj(&[(5, 182), (8, 183), (6, 184)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (5, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(9, 199), (5, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(5, 203), (6, 204)]),
    SCell::Obj(&[(7, 205), (5, 206), (8, 207)]),
    SCell::Obj(&[(7, 208), (5, 209)]),
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
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 237), (20, 238), (11, 239), (14, 240), (45, 241), (42, 242), (15, 243), (19, 244), (18, 245), (22, 246), (23, 247), (24, 248), (4, 249), (12, 250), (3, 251), (13, 252), (0, 253), (1, 254), (2, 255)]),
    SCell::Obj(&[(21, 256), (20, 257), (11, 258), (14, 259), (45, 260), (42, 261), (15, 262), (19, 263), (18, 264), (22, 265), (23, 266), (24, 267), (4, 268), (12, 269), (3, 270), (13, 271), (0, 272), (1, 273), (2, 274)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 297), (2, 298)]),
    SCell::Obj(&[(17, 299), (16, 300), (1, 301), (2, 302)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 303), (2, 304)]),
    SCell::Obj(&[(1, 305), (2, 306)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 307), (2, 308)]),
    SCell::Obj(&[(17, 309), (16, 310), (1, 311), (2, 312)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 313), (2, 314)]),
    SCell::Obj(&[(1, 315), (2, 316)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 210),
    (152, 211),
    (155, 212),
    (156, 213),
    (157, 214),
    (158, 215),
    (160, 216),
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
    (176, 217),
    (178, 218),
    (179, 219),
    (181, 220),
    (182, 221),
    (184, 222),
    (186, 223),
    (188, 224),
    (190, 225),
    (192, 226),
    (195, 227),
    (198, 228),
    (200, 229),
    (202, 230),
    (203, 231),
    (204, 232),
    (205, 233),
    (206, 234),
    (208, 235),
    (209, 236),
    (237, 275),
    (238, 276),
    (240, 277),
    (243, 278),
    (244, 279),
    (245, 280),
    (246, 281),
    (247, 282),
    (248, 283),
    (249, 284),
    (251, 285),
    (253, 116),
    (256, 286),
    (257, 287),
    (259, 288),
    (262, 289),
    (263, 290),
    (264, 291),
    (265, 292),
    (266, 293),
    (267, 294),
    (268, 295),
    (270, 296),
    (272, 116),
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
    (190, "balloon.tile"),
    (217, "big_chest.tile"),
    (209, "chest.if_not_fruit"),
    (211, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (203, "fake_wall.if_not_fruit"),
    (204, "fake_wall.tile"),
    (193, "fall_floor.tile"),
    (199, "fly_fruit.if_not_fruit"),
    (201, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (195, "fruit.if_not_fruit"),
    (197, "fruit.tile"),
    (185, "got_fruit[#3]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (206, "key.if_not_fruit"),
    (207, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (261, "objects[0].collideable"),
    (262, "objects[0].delay"),
    (682, "objects[0].flip.x"),
    (683, "objects[0].flip.y"),
    (684, "objects[0].hitbox.h"),
    (685, "objects[0].hitbox.w"),
    (686, "objects[0].hitbox.x"),
    (687, "objects[0].hitbox.y"),
    (688, "objects[0].rem.x"),
    (689, "objects[0].rem.y"),
    (271, "objects[0].solids"),
    (690, "objects[0].spd.x"),
    (691, "objects[0].spd.y"),
    (273, "objects[0].spr"),
    (274, "objects[0].state"),
    (692, "objects[0].target.x"),
    (693, "objects[0].target.y"),
    (169, "objects[0].type.tile"),
    (277, "objects[0].x"),
    (278, "objects[0].y"),
    (281, "objects[1].collideable"),
    (694, "objects[1].flip.x"),
    (695, "objects[1].flip.y"),
    (283, "objects[1].fly"),
    (696, "objects[1].hitbox.h"),
    (697, "objects[1].hitbox.w"),
    (698, "objects[1].hitbox.x"),
    (699, "objects[1].hitbox.y"),
    (700, "objects[1].rem.x"),
    (701, "objects[1].rem.y"),
    (291, "objects[1].solids"),
    (702, "objects[1].spd.x"),
    (703, "objects[1].spd.y"),
    (293, "objects[1].spr"),
    (294, "objects[1].start"),
    (295, "objects[1].step"),
    (297, "objects[1].x"),
    (298, "objects[1].y"),
    (301, "objects[2].collideable"),
    (704, "objects[2].flip.x"),
    (705, "objects[2].flip.y"),
    (706, "objects[2].hitbox.h"),
    (707, "objects[2].hitbox.w"),
    (708, "objects[2].hitbox.x"),
    (709, "objects[2].hitbox.y"),
    (710, "objects[2].rem.x"),
    (711, "objects[2].rem.y"),
    (310, "objects[2].solid"),
    (311, "objects[2].solids"),
    (712, "objects[2].spd.x"),
    (713, "objects[2].spd.y"),
    (313, "objects[2].spr"),
    (314, "objects[2].state"),
    (316, "objects[2].x"),
    (317, "objects[2].y"),
    (320, "objects[3].collideable"),
    (714, "objects[3].flip.x"),
    (715, "objects[3].flip.y"),
    (716, "objects[3].hitbox.h"),
    (717, "objects[3].hitbox.w"),
    (718, "objects[3].hitbox.x"),
    (719, "objects[3].hitbox.y"),
    (720, "objects[3].rem.x"),
    (721, "objects[3].rem.y"),
    (329, "objects[3].solid"),
    (330, "objects[3].solids"),
    (722, "objects[3].spd.x"),
    (723, "objects[3].spd.y"),
    (332, "objects[3].spr"),
    (333, "objects[3].state"),
    (335, "objects[3].x"),
    (336, "objects[3].y"),
    (339, "objects[4].collideable"),
    (724, "objects[4].flip.x"),
    (725, "objects[4].flip.y"),
    (726, "objects[4].hitbox.h"),
    (727, "objects[4].hitbox.w"),
    (728, "objects[4].hitbox.x"),
    (729, "objects[4].hitbox.y"),
    (730, "objects[4].rem.x"),
    (731, "objects[4].rem.y"),
    (348, "objects[4].solid"),
    (349, "objects[4].solids"),
    (732, "objects[4].spd.x"),
    (733, "objects[4].spd.y"),
    (351, "objects[4].spr"),
    (352, "objects[4].state"),
    (354, "objects[4].x"),
    (355, "objects[4].y"),
    (358, "objects[5].collideable"),
    (734, "objects[5].flip.x"),
    (735, "objects[5].flip.y"),
    (736, "objects[5].hitbox.h"),
    (737, "objects[5].hitbox.w"),
    (738, "objects[5].hitbox.x"),
    (739, "objects[5].hitbox.y"),
    (740, "objects[5].rem.x"),
    (741, "objects[5].rem.y"),
    (367, "objects[5].solid"),
    (368, "objects[5].solids"),
    (742, "objects[5].spd.x"),
    (743, "objects[5].spd.y"),
    (370, "objects[5].spr"),
    (371, "objects[5].state"),
    (373, "objects[5].x"),
    (374, "objects[5].y"),
    (377, "objects[6].collideable"),
    (744, "objects[6].flip.x"),
    (745, "objects[6].flip.y"),
    (746, "objects[6].hitbox.h"),
    (747, "objects[6].hitbox.w"),
    (748, "objects[6].hitbox.x"),
    (749, "objects[6].hitbox.y"),
    (750, "objects[6].rem.x"),
    (751, "objects[6].rem.y"),
    (386, "objects[6].solid"),
    (387, "objects[6].solids"),
    (752, "objects[6].spd.x"),
    (753, "objects[6].spd.y"),
    (389, "objects[6].spr"),
    (390, "objects[6].state"),
    (392, "objects[6].x"),
    (393, "objects[6].y"),
    (396, "objects[7].collideable"),
    (754, "objects[7].flip.x"),
    (755, "objects[7].flip.y"),
    (756, "objects[7].hitbox.h"),
    (757, "objects[7].hitbox.w"),
    (758, "objects[7].hitbox.x"),
    (759, "objects[7].hitbox.y"),
    (760, "objects[7].rem.x"),
    (761, "objects[7].rem.y"),
    (405, "objects[7].solid"),
    (406, "objects[7].solids"),
    (762, "objects[7].spd.x"),
    (763, "objects[7].spd.y"),
    (408, "objects[7].spr"),
    (409, "objects[7].state"),
    (411, "objects[7].x"),
    (412, "objects[7].y"),
    (415, "objects[8].collideable"),
    (764, "objects[8].flip.x"),
    (765, "objects[8].flip.y"),
    (766, "objects[8].hitbox.h"),
    (767, "objects[8].hitbox.w"),
    (768, "objects[8].hitbox.x"),
    (769, "objects[8].hitbox.y"),
    (770, "objects[8].rem.x"),
    (771, "objects[8].rem.y"),
    (424, "objects[8].solid"),
    (425, "objects[8].solids"),
    (772, "objects[8].spd.x"),
    (773, "objects[8].spd.y"),
    (427, "objects[8].spr"),
    (428, "objects[8].state"),
    (430, "objects[8].x"),
    (431, "objects[8].y"),
    (434, "objects[9].collideable"),
    (774, "objects[9].flip.x"),
    (775, "objects[9].flip.y"),
    (776, "objects[9].hitbox.h"),
    (777, "objects[9].hitbox.w"),
    (778, "objects[9].hitbox.x"),
    (779, "objects[9].hitbox.y"),
    (780, "objects[9].rem.x"),
    (781, "objects[9].rem.y"),
    (443, "objects[9].solid"),
    (444, "objects[9].solids"),
    (782, "objects[9].spd.x"),
    (783, "objects[9].spd.y"),
    (446, "objects[9].spr"),
    (447, "objects[9].state"),
    (449, "objects[9].x"),
    (450, "objects[9].y"),
    (453, "objects[10].collideable"),
    (784, "objects[10].flip.x"),
    (785, "objects[10].flip.y"),
    (786, "objects[10].hitbox.h"),
    (787, "objects[10].hitbox.w"),
    (788, "objects[10].hitbox.x"),
    (789, "objects[10].hitbox.y"),
    (790, "objects[10].rem.x"),
    (791, "objects[10].rem.y"),
    (462, "objects[10].solid"),
    (463, "objects[10].solids"),
    (792, "objects[10].spd.x"),
    (793, "objects[10].spd.y"),
    (465, "objects[10].spr"),
    (466, "objects[10].state"),
    (468, "objects[10].x"),
    (469, "objects[10].y"),
    (472, "objects[11].collideable"),
    (794, "objects[11].flip.x"),
    (795, "objects[11].flip.y"),
    (796, "objects[11].hitbox.h"),
    (797, "objects[11].hitbox.w"),
    (798, "objects[11].hitbox.x"),
    (799, "objects[11].hitbox.y"),
    (800, "objects[11].rem.x"),
    (801, "objects[11].rem.y"),
    (481, "objects[11].solid"),
    (482, "objects[11].solids"),
    (802, "objects[11].spd.x"),
    (803, "objects[11].spd.y"),
    (484, "objects[11].spr"),
    (485, "objects[11].state"),
    (487, "objects[11].x"),
    (488, "objects[11].y"),
    (491, "objects[12].collideable"),
    (804, "objects[12].flip.x"),
    (805, "objects[12].flip.y"),
    (806, "objects[12].hitbox.h"),
    (807, "objects[12].hitbox.w"),
    (808, "objects[12].hitbox.x"),
    (809, "objects[12].hitbox.y"),
    (810, "objects[12].rem.x"),
    (811, "objects[12].rem.y"),
    (500, "objects[12].solid"),
    (501, "objects[12].solids"),
    (812, "objects[12].spd.x"),
    (813, "objects[12].spd.y"),
    (503, "objects[12].spr"),
    (504, "objects[12].state"),
    (506, "objects[12].x"),
    (507, "objects[12].y"),
    (510, "objects[13].collideable"),
    (814, "objects[13].flip.x"),
    (815, "objects[13].flip.y"),
    (816, "objects[13].hitbox.h"),
    (817, "objects[13].hitbox.w"),
    (818, "objects[13].hitbox.x"),
    (819, "objects[13].hitbox.y"),
    (820, "objects[13].rem.x"),
    (821, "objects[13].rem.y"),
    (519, "objects[13].solid"),
    (520, "objects[13].solids"),
    (822, "objects[13].spd.x"),
    (823, "objects[13].spd.y"),
    (522, "objects[13].spr"),
    (523, "objects[13].state"),
    (525, "objects[13].x"),
    (526, "objects[13].y"),
    (43, "pause_player"),
    (171, "room.x"),
    (172, "room.y"),
    (85, "seconds"),
    (187, "spring.tile"),
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
    SCell::Arr(&[151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162, 163, 164]),
    SCell::Obj(&[(7, 165), (5, 166), (6, 167)]),
    SCell::Obj(&[(5, 168), (8, 169), (6, 170)]),
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
    SCell::Obj(&[(1, 171), (2, 172)]),
    SCell::Arr(&[173, 174, 175, 176, 177, 178, 179, 180, 181, 182]),
    SCell::Arr(&[183, 184, 185]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 186), (8, 187), (6, 188)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 189), (8, 190), (6, 191)]),
    SCell::Obj(&[(5, 192), (8, 193), (6, 194)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 195), (5, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(9, 199), (5, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(9, 203), (8, 204), (6, 205)]),
    SCell::Obj(&[(9, 206), (8, 207), (6, 208)]),
    SCell::Obj(&[(9, 209), (5, 210), (8, 211), (6, 212)]),
    SCell::Obj(&[(5, 213), (6, 214)]),
    SCell::Obj(&[(7, 215), (5, 216), (8, 217)]),
    SCell::Obj(&[(7, 218), (5, 219)]),
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
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 259), (20, 260), (11, 261), (35, 262), (14, 263), (15, 264), (19, 265), (18, 266), (22, 267), (23, 268), (24, 269), (4, 270), (12, 271), (3, 272), (13, 273), (25, 274), (34, 275), (0, 276), (1, 277), (2, 278)]),
    SCell::Obj(&[(21, 279), (20, 280), (11, 281), (14, 282), (39, 283), (15, 284), (19, 285), (18, 286), (22, 287), (23, 288), (24, 289), (4, 290), (12, 291), (3, 292), (13, 293), (38, 294), (40, 295), (0, 296), (1, 297), (2, 298)]),
    SCell::Obj(&[(21, 299), (20, 300), (11, 301), (14, 302), (15, 303), (19, 304), (18, 305), (22, 306), (23, 307), (24, 308), (4, 309), (43, 310), (12, 311), (3, 312), (13, 313), (25, 314), (0, 315), (1, 316), (2, 317)]),
    SCell::Obj(&[(21, 318), (20, 319), (11, 320), (14, 321), (15, 322), (19, 323), (18, 324), (22, 325), (23, 326), (24, 327), (4, 328), (43, 329), (12, 330), (3, 331), (13, 332), (25, 333), (0, 334), (1, 335), (2, 336)]),
    SCell::Obj(&[(21, 337), (20, 338), (11, 339), (14, 340), (15, 341), (19, 342), (18, 343), (22, 344), (23, 345), (24, 346), (4, 347), (43, 348), (12, 349), (3, 350), (13, 351), (25, 352), (0, 353), (1, 354), (2, 355)]),
    SCell::Obj(&[(21, 356), (20, 357), (11, 358), (14, 359), (15, 360), (19, 361), (18, 362), (22, 363), (23, 364), (24, 365), (4, 366), (43, 367), (12, 368), (3, 369), (13, 370), (25, 371), (0, 372), (1, 373), (2, 374)]),
    SCell::Obj(&[(21, 375), (20, 376), (11, 377), (14, 378), (15, 379), (19, 380), (18, 381), (22, 382), (23, 383), (24, 384), (4, 385), (43, 386), (12, 387), (3, 388), (13, 389), (25, 390), (0, 391), (1, 392), (2, 393)]),
    SCell::Obj(&[(21, 394), (20, 395), (11, 396), (14, 397), (15, 398), (19, 399), (18, 400), (22, 401), (23, 402), (24, 403), (4, 404), (43, 405), (12, 406), (3, 407), (13, 408), (25, 409), (0, 410), (1, 411), (2, 412)]),
    SCell::Obj(&[(21, 413), (20, 414), (11, 415), (14, 416), (15, 417), (19, 418), (18, 419), (22, 420), (23, 421), (24, 422), (4, 423), (43, 424), (12, 425), (3, 426), (13, 427), (25, 428), (0, 429), (1, 430), (2, 431)]),
    SCell::Obj(&[(21, 432), (20, 433), (11, 434), (14, 435), (15, 436), (19, 437), (18, 438), (22, 439), (23, 440), (24, 441), (4, 442), (43, 443), (12, 444), (3, 445), (13, 446), (25, 447), (0, 448), (1, 449), (2, 450)]),
    SCell::Obj(&[(21, 451), (20, 452), (11, 453), (14, 454), (15, 455), (19, 456), (18, 457), (22, 458), (23, 459), (24, 460), (4, 461), (43, 462), (12, 463), (3, 464), (13, 465), (25, 466), (0, 467), (1, 468), (2, 469)]),
    SCell::Obj(&[(21, 470), (20, 471), (11, 472), (14, 473), (15, 474), (19, 475), (18, 476), (22, 477), (23, 478), (24, 479), (4, 480), (43, 481), (12, 482), (3, 483), (13, 484), (25, 485), (0, 486), (1, 487), (2, 488)]),
    SCell::Obj(&[(21, 489), (20, 490), (11, 491), (14, 492), (15, 493), (19, 494), (18, 495), (22, 496), (23, 497), (24, 498), (4, 499), (43, 500), (12, 501), (3, 502), (13, 503), (25, 504), (0, 505), (1, 506), (2, 507)]),
    SCell::Obj(&[(21, 508), (20, 509), (11, 510), (14, 511), (15, 512), (19, 513), (18, 514), (22, 515), (23, 516), (24, 517), (4, 518), (43, 519), (12, 520), (3, 521), (13, 522), (25, 523), (0, 524), (1, 525), (2, 526)]),
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
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Clo(26, &[220]),
    SCell::Clo(25, &[220]),
    SCell::Obj(&[(1, 682), (2, 683)]),
    SCell::Obj(&[(17, 684), (16, 685), (1, 686), (2, 687)]),
    SCell::Clo(24, &[220]),
    SCell::Clo(23, &[220]),
    SCell::Clo(27, &[220]),
    SCell::Clo(28, &[220]),
    SCell::Clo(29, &[220]),
    SCell::Obj(&[(1, 688), (2, 689)]),
    SCell::Obj(&[(1, 690), (2, 691)]),
    SCell::Obj(&[(1, 692), (2, 693)]),
    SCell::Clo(26, &[221]),
    SCell::Clo(25, &[221]),
    SCell::Obj(&[(1, 694), (2, 695)]),
    SCell::Obj(&[(17, 696), (16, 697), (1, 698), (2, 699)]),
    SCell::Clo(24, &[221]),
    SCell::Clo(23, &[221]),
    SCell::Clo(27, &[221]),
    SCell::Clo(28, &[221]),
    SCell::Clo(29, &[221]),
    SCell::Obj(&[(1, 700), (2, 701)]),
    SCell::Obj(&[(1, 702), (2, 703)]),
    SCell::Clo(26, &[222]),
    SCell::Clo(25, &[222]),
    SCell::Obj(&[(1, 704), (2, 705)]),
    SCell::Obj(&[(17, 706), (16, 707), (1, 708), (2, 709)]),
    SCell::Clo(24, &[222]),
    SCell::Clo(23, &[222]),
    SCell::Clo(27, &[222]),
    SCell::Clo(28, &[222]),
    SCell::Clo(29, &[222]),
    SCell::Obj(&[(1, 710), (2, 711)]),
    SCell::Obj(&[(1, 712), (2, 713)]),
    SCell::Clo(26, &[223]),
    SCell::Clo(25, &[223]),
    SCell::Obj(&[(1, 714), (2, 715)]),
    SCell::Obj(&[(17, 716), (16, 717), (1, 718), (2, 719)]),
    SCell::Clo(24, &[223]),
    SCell::Clo(23, &[223]),
    SCell::Clo(27, &[223]),
    SCell::Clo(28, &[223]),
    SCell::Clo(29, &[223]),
    SCell::Obj(&[(1, 720), (2, 721)]),
    SCell::Obj(&[(1, 722), (2, 723)]),
    SCell::Clo(26, &[224]),
    SCell::Clo(25, &[224]),
    SCell::Obj(&[(1, 724), (2, 725)]),
    SCell::Obj(&[(17, 726), (16, 727), (1, 728), (2, 729)]),
    SCell::Clo(24, &[224]),
    SCell::Clo(23, &[224]),
    SCell::Clo(27, &[224]),
    SCell::Clo(28, &[224]),
    SCell::Clo(29, &[224]),
    SCell::Obj(&[(1, 730), (2, 731)]),
    SCell::Obj(&[(1, 732), (2, 733)]),
    SCell::Clo(26, &[225]),
    SCell::Clo(25, &[225]),
    SCell::Obj(&[(1, 734), (2, 735)]),
    SCell::Obj(&[(17, 736), (16, 737), (1, 738), (2, 739)]),
    SCell::Clo(24, &[225]),
    SCell::Clo(23, &[225]),
    SCell::Clo(27, &[225]),
    SCell::Clo(28, &[225]),
    SCell::Clo(29, &[225]),
    SCell::Obj(&[(1, 740), (2, 741)]),
    SCell::Obj(&[(1, 742), (2, 743)]),
    SCell::Clo(26, &[226]),
    SCell::Clo(25, &[226]),
    SCell::Obj(&[(1, 744), (2, 745)]),
    SCell::Obj(&[(17, 746), (16, 747), (1, 748), (2, 749)]),
    SCell::Clo(24, &[226]),
    SCell::Clo(23, &[226]),
    SCell::Clo(27, &[226]),
    SCell::Clo(28, &[226]),
    SCell::Clo(29, &[226]),
    SCell::Obj(&[(1, 750), (2, 751)]),
    SCell::Obj(&[(1, 752), (2, 753)]),
    SCell::Clo(26, &[227]),
    SCell::Clo(25, &[227]),
    SCell::Obj(&[(1, 754), (2, 755)]),
    SCell::Obj(&[(17, 756), (16, 757), (1, 758), (2, 759)]),
    SCell::Clo(24, &[227]),
    SCell::Clo(23, &[227]),
    SCell::Clo(27, &[227]),
    SCell::Clo(28, &[227]),
    SCell::Clo(29, &[227]),
    SCell::Obj(&[(1, 760), (2, 761)]),
    SCell::Obj(&[(1, 762), (2, 763)]),
    SCell::Clo(26, &[228]),
    SCell::Clo(25, &[228]),
    SCell::Obj(&[(1, 764), (2, 765)]),
    SCell::Obj(&[(17, 766), (16, 767), (1, 768), (2, 769)]),
    SCell::Clo(24, &[228]),
    SCell::Clo(23, &[228]),
    SCell::Clo(27, &[228]),
    SCell::Clo(28, &[228]),
    SCell::Clo(29, &[228]),
    SCell::Obj(&[(1, 770), (2, 771)]),
    SCell::Obj(&[(1, 772), (2, 773)]),
    SCell::Clo(26, &[229]),
    SCell::Clo(25, &[229]),
    SCell::Obj(&[(1, 774), (2, 775)]),
    SCell::Obj(&[(17, 776), (16, 777), (1, 778), (2, 779)]),
    SCell::Clo(24, &[229]),
    SCell::Clo(23, &[229]),
    SCell::Clo(27, &[229]),
    SCell::Clo(28, &[229]),
    SCell::Clo(29, &[229]),
    SCell::Obj(&[(1, 780), (2, 781)]),
    SCell::Obj(&[(1, 782), (2, 783)]),
    SCell::Clo(26, &[230]),
    SCell::Clo(25, &[230]),
    SCell::Obj(&[(1, 784), (2, 785)]),
    SCell::Obj(&[(17, 786), (16, 787), (1, 788), (2, 789)]),
    SCell::Clo(24, &[230]),
    SCell::Clo(23, &[230]),
    SCell::Clo(27, &[230]),
    SCell::Clo(28, &[230]),
    SCell::Clo(29, &[230]),
    SCell::Obj(&[(1, 790), (2, 791)]),
    SCell::Obj(&[(1, 792), (2, 793)]),
    SCell::Clo(26, &[231]),
    SCell::Clo(25, &[231]),
    SCell::Obj(&[(1, 794), (2, 795)]),
    SCell::Obj(&[(17, 796), (16, 797), (1, 798), (2, 799)]),
    SCell::Clo(24, &[231]),
    SCell::Clo(23, &[231]),
    SCell::Clo(27, &[231]),
    SCell::Clo(28, &[231]),
    SCell::Clo(29, &[231]),
    SCell::Obj(&[(1, 800), (2, 801)]),
    SCell::Obj(&[(1, 802), (2, 803)]),
    SCell::Clo(26, &[232]),
    SCell::Clo(25, &[232]),
    SCell::Obj(&[(1, 804), (2, 805)]),
    SCell::Obj(&[(17, 806), (16, 807), (1, 808), (2, 809)]),
    SCell::Clo(24, &[232]),
    SCell::Clo(23, &[232]),
    SCell::Clo(27, &[232]),
    SCell::Clo(28, &[232]),
    SCell::Clo(29, &[232]),
    SCell::Obj(&[(1, 810), (2, 811)]),
    SCell::Obj(&[(1, 812), (2, 813)]),
    SCell::Clo(26, &[233]),
    SCell::Clo(25, &[233]),
    SCell::Obj(&[(1, 814), (2, 815)]),
    SCell::Obj(&[(17, 816), (16, 817), (1, 818), (2, 819)]),
    SCell::Clo(24, &[233]),
    SCell::Clo(23, &[233]),
    SCell::Clo(27, &[233]),
    SCell::Clo(28, &[233]),
    SCell::Clo(29, &[233]),
    SCell::Obj(&[(1, 820), (2, 821)]),
    SCell::Obj(&[(1, 822), (2, 823)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 220),
    (152, 221),
    (153, 222),
    (154, 223),
    (155, 224),
    (156, 225),
    (157, 226),
    (158, 227),
    (159, 228),
    (160, 229),
    (161, 230),
    (162, 231),
    (163, 232),
    (164, 233),
    (165, 234),
    (166, 235),
    (167, 236),
    (168, 237),
    (170, 238),
    (173, 94),
    (174, 116),
    (175, 118),
    (176, 119),
    (177, 121),
    (178, 122),
    (179, 123),
    (180, 124),
    (181, 125),
    (182, 127),
    (186, 239),
    (188, 240),
    (189, 241),
    (191, 242),
    (192, 243),
    (194, 244),
    (196, 245),
    (198, 246),
    (200, 247),
    (202, 248),
    (205, 249),
    (208, 250),
    (210, 251),
    (212, 252),
    (213, 253),
    (214, 254),
    (215, 255),
    (216, 256),
    (218, 257),
    (219, 258),
    (259, 527),
    (260, 528),
    (263, 529),
    (264, 530),
    (265, 531),
    (266, 532),
    (267, 533),
    (268, 534),
    (269, 535),
    (270, 536),
    (272, 537),
    (275, 538),
    (276, 94),
    (279, 539),
    (280, 540),
    (282, 541),
    (284, 542),
    (285, 543),
    (286, 544),
    (287, 545),
    (288, 546),
    (289, 547),
    (290, 548),
    (292, 549),
    (296, 122),
    (299, 550),
    (300, 551),
    (302, 552),
    (303, 553),
    (304, 554),
    (305, 555),
    (306, 556),
    (307, 557),
    (308, 558),
    (309, 559),
    (312, 560),
    (315, 119),
    (318, 561),
    (319, 562),
    (321, 563),
    (322, 564),
    (323, 565),
    (324, 566),
    (325, 567),
    (326, 568),
    (327, 569),
    (328, 570),
    (331, 571),
    (334, 119),
    (337, 572),
    (338, 573),
    (340, 574),
    (341, 575),
    (342, 576),
    (343, 577),
    (344, 578),
    (345, 579),
    (346, 580),
    (347, 581),
    (350, 582),
    (353, 119),
    (356, 583),
    (357, 584),
    (359, 585),
    (360, 586),
    (361, 587),
    (362, 588),
    (363, 589),
    (364, 590),
    (365, 591),
    (366, 592),
    (369, 593),
    (372, 119),
    (375, 594),
    (376, 595),
    (378, 596),
    (379, 597),
    (380, 598),
    (381, 599),
    (382, 600),
    (383, 601),
    (384, 602),
    (385, 603),
    (388, 604),
    (391, 119),
    (394, 605),
    (395, 606),
    (397, 607),
    (398, 608),
    (399, 609),
    (400, 610),
    (401, 611),
    (402, 612),
    (403, 613),
    (404, 614),
    (407, 615),
    (410, 119),
    (413, 616),
    (414, 617),
    (416, 618),
    (417, 619),
    (418, 620),
    (419, 621),
    (420, 622),
    (421, 623),
    (422, 624),
    (423, 625),
    (426, 626),
    (429, 119),
    (432, 627),
    (433, 628),
    (435, 629),
    (436, 630),
    (437, 631),
    (438, 632),
    (439, 633),
    (440, 634),
    (441, 635),
    (442, 636),
    (445, 637),
    (448, 119),
    (451, 638),
    (452, 639),
    (454, 640),
    (455, 641),
    (456, 642),
    (457, 643),
    (458, 644),
    (459, 645),
    (460, 646),
    (461, 647),
    (464, 648),
    (467, 119),
    (470, 649),
    (471, 650),
    (473, 651),
    (474, 652),
    (475, 653),
    (476, 654),
    (477, 655),
    (478, 656),
    (479, 657),
    (480, 658),
    (483, 659),
    (486, 119),
    (489, 660),
    (490, 661),
    (492, 662),
    (493, 663),
    (494, 664),
    (495, 665),
    (496, 666),
    (497, 667),
    (498, 668),
    (499, 669),
    (502, 670),
    (505, 119),
    (508, 671),
    (509, 672),
    (511, 673),
    (512, 674),
    (513, 675),
    (514, 676),
    (515, 677),
    (516, 678),
    (517, 679),
    (518, 680),
    (521, 681),
    (524, 119),
];

/// Outcome 2's button-INDEPENDENT values.
/// Values shared by every button assignment.
/// COMPILE-TIME CONSTANTS are absent: their column is
/// written once when the block is built, so there is
/// nothing to carry per lane.
pub struct KShared2 {
    pub c84: ZN,
    pub c86: ZN,
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

// ---------------- outcome 3 ----------------
/// (cell, path) - where each computed value goes.
pub const OUT_SLOTS_3: &[(u32, &str)] = &[
    (180, "balloon.tile"),
    (207, "big_chest.tile"),
    (199, "chest.if_not_fruit"),
    (201, "chest.tile"),
    (87, "deaths"),
    (39, "delay_restart"),
    (193, "fake_wall.if_not_fruit"),
    (194, "fake_wall.tile"),
    (183, "fall_floor.tile"),
    (189, "fly_fruit.if_not_fruit"),
    (191, "fly_fruit.tile"),
    (84, "frames"),
    (20, "freeze"),
    (185, "fruit.if_not_fruit"),
    (187, "fruit.tile"),
    (175, "got_fruit[#3]"),
    (41, "has_dashed"),
    (42, "has_key"),
    (49, "k_dash"),
    (47, "k_down"),
    (48, "k_jump"),
    (44, "k_left"),
    (45, "k_right"),
    (46, "k_up"),
    (196, "key.if_not_fruit"),
    (197, "key.tile"),
    (88, "max_djump"),
    (86, "minutes"),
    (240, "objects[0].collideable"),
    (241, "objects[0].delay"),
    (330, "objects[0].flip.x"),
    (331, "objects[0].flip.y"),
    (332, "objects[0].hitbox.h"),
    (333, "objects[0].hitbox.w"),
    (334, "objects[0].hitbox.x"),
    (335, "objects[0].hitbox.y"),
    (336, "objects[0].rem.x"),
    (337, "objects[0].rem.y"),
    (250, "objects[0].solids"),
    (338, "objects[0].spd.x"),
    (339, "objects[0].spd.y"),
    (252, "objects[0].spr"),
    (253, "objects[0].state"),
    (340, "objects[0].target.x"),
    (341, "objects[0].target.y"),
    (159, "objects[0].type.tile"),
    (256, "objects[0].x"),
    (257, "objects[0].y"),
    (260, "objects[1].collideable"),
    (342, "objects[1].flip.x"),
    (343, "objects[1].flip.y"),
    (262, "objects[1].hide_for"),
    (263, "objects[1].hide_in"),
    (344, "objects[1].hitbox.h"),
    (345, "objects[1].hitbox.w"),
    (346, "objects[1].hitbox.x"),
    (347, "objects[1].hitbox.y"),
    (348, "objects[1].rem.x"),
    (349, "objects[1].rem.y"),
    (271, "objects[1].solids"),
    (350, "objects[1].spd.x"),
    (351, "objects[1].spd.y"),
    (273, "objects[1].spr"),
    (177, "objects[1].type.tile"),
    (275, "objects[1].x"),
    (276, "objects[1].y"),
    (279, "objects[2].collideable"),
    (352, "objects[2].flip.x"),
    (353, "objects[2].flip.y"),
    (281, "objects[2].hide_for"),
    (282, "objects[2].hide_in"),
    (354, "objects[2].hitbox.h"),
    (355, "objects[2].hitbox.w"),
    (356, "objects[2].hitbox.x"),
    (357, "objects[2].hitbox.y"),
    (358, "objects[2].rem.x"),
    (359, "objects[2].rem.y"),
    (290, "objects[2].solids"),
    (360, "objects[2].spd.x"),
    (361, "objects[2].spd.y"),
    (292, "objects[2].spr"),
    (294, "objects[2].x"),
    (295, "objects[2].y"),
    (43, "pause_player"),
    (161, "room.x"),
    (162, "room.y"),
    (85, "seconds"),
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
    SCell::Arr(&[173, 174, 175]),
    SCell::Clo(61, &[]),
    SCell::Clo(60, &[]),
    SCell::Clo(59, &[]),
    SCell::Clo(58, &[]),
    SCell::Clo(57, &[]),
    SCell::Obj(&[(5, 176), (8, 177), (6, 178)]),
    SCell::Clo(49, &[]),
    SCell::Obj(&[(5, 179), (8, 180), (6, 181)]),
    SCell::Obj(&[(5, 182), (8, 183), (6, 184)]),
    SCell::Clo(44, &[]),
    SCell::Obj(&[(9, 185), (5, 186), (8, 187), (6, 188)]),
    SCell::Obj(&[(9, 189), (5, 190), (8, 191), (6, 192)]),
    SCell::Obj(&[(9, 193), (8, 194), (6, 195)]),
    SCell::Obj(&[(9, 196), (8, 197), (6, 198)]),
    SCell::Obj(&[(9, 199), (5, 200), (8, 201), (6, 202)]),
    SCell::Obj(&[(5, 203), (6, 204)]),
    SCell::Obj(&[(7, 205), (5, 206), (8, 207)]),
    SCell::Obj(&[(7, 208), (5, 209)]),
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
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Obj(&[(21, 238), (20, 239), (11, 240), (35, 241), (14, 242), (15, 243), (19, 244), (18, 245), (22, 246), (23, 247), (24, 248), (4, 249), (12, 250), (3, 251), (13, 252), (25, 253), (34, 254), (0, 255), (1, 256), (2, 257)]),
    SCell::Obj(&[(21, 258), (20, 259), (11, 260), (14, 261), (45, 262), (42, 263), (15, 264), (19, 265), (18, 266), (22, 267), (23, 268), (24, 269), (4, 270), (12, 271), (3, 272), (13, 273), (0, 274), (1, 275), (2, 276)]),
    SCell::Obj(&[(21, 277), (20, 278), (11, 279), (14, 280), (45, 281), (42, 282), (15, 283), (19, 284), (18, 285), (22, 286), (23, 287), (24, 288), (4, 289), (12, 290), (3, 291), (13, 292), (0, 293), (1, 294), (2, 295)]),
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
    SCell::Clo(26, &[210]),
    SCell::Clo(25, &[210]),
    SCell::Obj(&[(1, 330), (2, 331)]),
    SCell::Obj(&[(17, 332), (16, 333), (1, 334), (2, 335)]),
    SCell::Clo(24, &[210]),
    SCell::Clo(23, &[210]),
    SCell::Clo(27, &[210]),
    SCell::Clo(28, &[210]),
    SCell::Clo(29, &[210]),
    SCell::Obj(&[(1, 336), (2, 337)]),
    SCell::Obj(&[(1, 338), (2, 339)]),
    SCell::Obj(&[(1, 340), (2, 341)]),
    SCell::Clo(26, &[211]),
    SCell::Clo(25, &[211]),
    SCell::Obj(&[(1, 342), (2, 343)]),
    SCell::Obj(&[(17, 344), (16, 345), (1, 346), (2, 347)]),
    SCell::Clo(24, &[211]),
    SCell::Clo(23, &[211]),
    SCell::Clo(27, &[211]),
    SCell::Clo(28, &[211]),
    SCell::Clo(29, &[211]),
    SCell::Obj(&[(1, 348), (2, 349)]),
    SCell::Obj(&[(1, 350), (2, 351)]),
    SCell::Clo(26, &[212]),
    SCell::Clo(25, &[212]),
    SCell::Obj(&[(1, 352), (2, 353)]),
    SCell::Obj(&[(17, 354), (16, 355), (1, 356), (2, 357)]),
    SCell::Clo(24, &[212]),
    SCell::Clo(23, &[212]),
    SCell::Clo(27, &[212]),
    SCell::Clo(28, &[212]),
    SCell::Clo(29, &[212]),
    SCell::Obj(&[(1, 358), (2, 359)]),
    SCell::Obj(&[(1, 360), (2, 361)]),
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
    SCell::Val,
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
    (151, 210),
    (152, 211),
    (153, 212),
    (155, 213),
    (156, 214),
    (157, 215),
    (158, 216),
    (160, 217),
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
    (176, 218),
    (178, 219),
    (179, 220),
    (181, 221),
    (182, 222),
    (184, 223),
    (186, 224),
    (188, 225),
    (190, 226),
    (192, 227),
    (195, 228),
    (198, 229),
    (200, 230),
    (202, 231),
    (203, 232),
    (204, 233),
    (205, 234),
    (206, 235),
    (208, 236),
    (209, 237),
    (238, 296),
    (239, 297),
    (242, 298),
    (243, 299),
    (244, 300),
    (245, 301),
    (246, 302),
    (247, 303),
    (248, 304),
    (249, 305),
    (251, 306),
    (254, 307),
    (255, 94),
    (258, 308),
    (259, 309),
    (261, 310),
    (264, 311),
    (265, 312),
    (266, 313),
    (267, 314),
    (268, 315),
    (269, 316),
    (270, 317),
    (272, 318),
    (274, 116),
    (277, 319),
    (278, 320),
    (280, 321),
    (283, 322),
    (284, 323),
    (285, 324),
    (286, 325),
    (287, 326),
    (288, 327),
    (289, 328),
    (291, 329),
    (293, 116),
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
    pub c241: ZN,
    pub c337: ZN,
    pub c339: ZN,
    pub c252: ZN,
    pub c253: ZN,
    pub c257: ZN,
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
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[207] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[199] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[175] = Col::U(AV::Bool(true));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[240] = Col::U(AV::Bool(true));
    b.cols[336] = Col::U(AV::Bool(false));
    b.cols[337] = Col::U(AV::Bool(false));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[243] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[338] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[339] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[340] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[342] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[343] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[251] = Col::U(AV::Bool(true));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[253] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[259] = Col::U(AV::Bool(true));
    b.cols[346] = Col::U(AV::Bool(false));
    b.cols[347] = Col::U(AV::Bool(false));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[353] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[270] = Col::U(AV::Bool(true));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[272] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[278] = Col::U(AV::Bool(true));
    b.cols[356] = Col::N(Vec::new());
    b.cols[357] = Col::N(Vec::new());
    b.cols[280] = Col::N(Vec::new());
    b.cols[358] = Col::N(Vec::new());
    b.cols[359] = Col::N(Vec::new());
    b.cols[282] = Col::N(Vec::new());
    b.cols[283] = Col::N(Vec::new());
    b.cols[360] = Col::V(Vec::new());
    b.cols[361] = Col::U(AV::Bool(false));
    b.cols[285] = Col::N(Vec::new());
    b.cols[362] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[363] = Col::U(AV::Num(P8::from_raw(393216i32)));
    b.cols[364] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[365] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[292] = Col::V(Vec::new());
    b.cols[293] = Col::V(Vec::new());
    b.cols[366] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[295] = Col::U(AV::Bool(true));
    b.cols[368] = Col::N(Vec::new());
    b.cols[369] = Col::N(Vec::new());
    b.cols[299] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[300] = Col::N(Vec::new());
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
        if let Col::N(v) = &mut acc.cols[356] { v.push(kv.c356.lane(i)); }
        if let Col::N(v) = &mut acc.cols[357] { v.push(kv.c357.lane(i)); }
        if let Col::N(v) = &mut acc.cols[280] { v.push(kv.c280.lane(i)); }
        if let Col::N(v) = &mut acc.cols[358] { v.push(kv.c358.lane(i)); }
        if let Col::N(v) = &mut acc.cols[359] { v.push(kv.c359.lane(i)); }
        if let Col::N(v) = &mut acc.cols[282] { v.push(kv.c282.lane(i)); }
        if let Col::N(v) = &mut acc.cols[283] { v.push(kv.c283.lane(i)); }
        if let Col::V(v) = &mut acc.cols[360] {
            v.push(if kv.c360.known & (1 << i) != 0 {
                AV::Bool(kv.c360.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[285] { v.push(kv.c285.lane(i)); }
        if let Col::V(v) = &mut acc.cols[292] {
            v.push(if kv.c292.known & (1 << i) != 0 {
                AV::Bool(kv.c292.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::V(v) = &mut acc.cols[293] {
            v.push(if kv.c293.known & (1 << i) != 0 {
                AV::Bool(kv.c293.val & (1 << i) != 0)
            } else { AV::UBool });
        }
        if let Col::N(v) = &mut acc.cols[368] { v.push(kv.c368.lane(i)); }
        if let Col::N(v) = &mut acc.cols[369] { v.push(kv.c369.lane(i)); }
        if let Col::N(v) = &mut acc.cols[300] { v.push(sh.c300.lane(i)); }
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
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[207] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[199] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::U(AV::Num(P8::from_raw(983040i32)));
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[175] = Col::U(AV::Bool(true));
    b.cols[41] = Col::V(Vec::new());
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[239] = Col::U(AV::Bool(true));
    b.cols[297] = Col::U(AV::Bool(false));
    b.cols[298] = Col::U(AV::Bool(false));
    b.cols[241] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[242] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[299] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[300] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[301] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[302] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[303] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[304] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[250] = Col::U(AV::Bool(true));
    b.cols[305] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[306] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[252] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[254] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[255] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[258] = Col::U(AV::Bool(true));
    b.cols[307] = Col::U(AV::Bool(false));
    b.cols[308] = Col::U(AV::Bool(false));
    b.cols[260] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[261] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[309] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[310] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[311] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[312] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[269] = Col::U(AV::Bool(true));
    b.cols[315] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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

/// An EMPTY accumulator with outcome 2's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append2`.
pub fn acc2(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_2, OUT_GLOBALS_2, OUT_PTRS_2, 0, cart, cache);
    b.cols[190] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[217] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[209] = Col::U(AV::Bool(true));
    b.cols[211] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[203] = Col::U(AV::Bool(true));
    b.cols[204] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[193] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[199] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[195] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[206] = Col::U(AV::Bool(true));
    b.cols[207] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[261] = Col::U(AV::Bool(true));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[682] = Col::U(AV::Bool(false));
    b.cols[683] = Col::U(AV::Bool(false));
    b.cols[684] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[685] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[686] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[687] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[688] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[689] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Bool(false));
    b.cols[690] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[691] = Col::U(AV::Num(P8::from_raw(-262144i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[274] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[692] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[693] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[169] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[277] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[278] = Col::U(AV::Num(P8::from_raw(8388608i32)));
    b.cols[281] = Col::U(AV::Bool(true));
    b.cols[694] = Col::U(AV::Bool(false));
    b.cols[695] = Col::U(AV::Bool(false));
    b.cols[283] = Col::U(AV::Bool(false));
    b.cols[696] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[697] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[698] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[699] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[700] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[701] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[291] = Col::U(AV::Bool(false));
    b.cols[702] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[703] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[293] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[294] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[295] = Col::U(AV::Num(P8::from_raw(32768i32)));
    b.cols[297] = Col::U(AV::Num(P8::from_raw(1048576i32)));
    b.cols[298] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[301] = Col::U(AV::Bool(true));
    b.cols[704] = Col::U(AV::Bool(false));
    b.cols[705] = Col::U(AV::Bool(false));
    b.cols[706] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[707] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[708] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[709] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[710] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[711] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[310] = Col::U(AV::Bool(true));
    b.cols[311] = Col::U(AV::Bool(true));
    b.cols[712] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[713] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[313] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[314] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[316] = Col::U(AV::Num(P8::from_raw(2097152i32)));
    b.cols[317] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[320] = Col::U(AV::Bool(true));
    b.cols[714] = Col::U(AV::Bool(false));
    b.cols[715] = Col::U(AV::Bool(false));
    b.cols[716] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[717] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[718] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[719] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[720] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[721] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[329] = Col::U(AV::Bool(true));
    b.cols[330] = Col::U(AV::Bool(true));
    b.cols[722] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[723] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[332] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[335] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[336] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[339] = Col::U(AV::Bool(true));
    b.cols[724] = Col::U(AV::Bool(false));
    b.cols[725] = Col::U(AV::Bool(false));
    b.cols[726] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[727] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[728] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[729] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[730] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[731] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Bool(true));
    b.cols[349] = Col::U(AV::Bool(true));
    b.cols[732] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[733] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[352] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[358] = Col::U(AV::Bool(true));
    b.cols[734] = Col::U(AV::Bool(false));
    b.cols[735] = Col::U(AV::Bool(false));
    b.cols[736] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[737] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[738] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[739] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[740] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[741] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[367] = Col::U(AV::Bool(true));
    b.cols[368] = Col::U(AV::Bool(true));
    b.cols[742] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[743] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[370] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[371] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[373] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[374] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[377] = Col::U(AV::Bool(true));
    b.cols[744] = Col::U(AV::Bool(false));
    b.cols[745] = Col::U(AV::Bool(false));
    b.cols[746] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[747] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[748] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[749] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[750] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[751] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[386] = Col::U(AV::Bool(true));
    b.cols[387] = Col::U(AV::Bool(true));
    b.cols[752] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[753] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[389] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[390] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[392] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[393] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[396] = Col::U(AV::Bool(true));
    b.cols[754] = Col::U(AV::Bool(false));
    b.cols[755] = Col::U(AV::Bool(false));
    b.cols[756] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[757] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[758] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[759] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[760] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[761] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[405] = Col::U(AV::Bool(true));
    b.cols[406] = Col::U(AV::Bool(true));
    b.cols[762] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[763] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[408] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[409] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[411] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[412] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[415] = Col::U(AV::Bool(true));
    b.cols[764] = Col::U(AV::Bool(false));
    b.cols[765] = Col::U(AV::Bool(false));
    b.cols[766] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[767] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[768] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[769] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[770] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[771] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[424] = Col::U(AV::Bool(true));
    b.cols[425] = Col::U(AV::Bool(true));
    b.cols[772] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[773] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[427] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[428] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[430] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[431] = Col::U(AV::Num(P8::from_raw(4718592i32)));
    b.cols[434] = Col::U(AV::Bool(true));
    b.cols[774] = Col::U(AV::Bool(false));
    b.cols[775] = Col::U(AV::Bool(false));
    b.cols[776] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[777] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[778] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[779] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[780] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[781] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[443] = Col::U(AV::Bool(true));
    b.cols[444] = Col::U(AV::Bool(true));
    b.cols[782] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[783] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[446] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[447] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[449] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[450] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[453] = Col::U(AV::Bool(true));
    b.cols[784] = Col::U(AV::Bool(false));
    b.cols[785] = Col::U(AV::Bool(false));
    b.cols[786] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[787] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[788] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[789] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[790] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[791] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[462] = Col::U(AV::Bool(true));
    b.cols[463] = Col::U(AV::Bool(true));
    b.cols[792] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[793] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[465] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[466] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[468] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[469] = Col::U(AV::Num(P8::from_raw(3145728i32)));
    b.cols[472] = Col::U(AV::Bool(true));
    b.cols[794] = Col::U(AV::Bool(false));
    b.cols[795] = Col::U(AV::Bool(false));
    b.cols[796] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[797] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[798] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[799] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[800] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[801] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[481] = Col::U(AV::Bool(true));
    b.cols[482] = Col::U(AV::Bool(true));
    b.cols[802] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[803] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[484] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[485] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[487] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[488] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[491] = Col::U(AV::Bool(true));
    b.cols[804] = Col::U(AV::Bool(false));
    b.cols[805] = Col::U(AV::Bool(false));
    b.cols[806] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[807] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[808] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[809] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[810] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[811] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[500] = Col::U(AV::Bool(true));
    b.cols[501] = Col::U(AV::Bool(true));
    b.cols[812] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[813] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[503] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[504] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[506] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[507] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[510] = Col::U(AV::Bool(true));
    b.cols[814] = Col::U(AV::Bool(false));
    b.cols[815] = Col::U(AV::Bool(false));
    b.cols[816] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[817] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[818] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[819] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[820] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[821] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[519] = Col::U(AV::Bool(true));
    b.cols[520] = Col::U(AV::Bool(true));
    b.cols[822] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[823] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[522] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[523] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[525] = Col::U(AV::Num(P8::from_raw(7864320i32)));
    b.cols[526] = Col::U(AV::Num(P8::from_raw(5767168i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[171] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[172] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1179648i32)));
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

/// An EMPTY accumulator with outcome 3's shape: structure,
/// globals and pointers from the constants above, computed
/// columns starting empty and growing by `append3`.
pub fn acc3(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {
    let mut b = build_block(OUT_SHAPE_3, OUT_GLOBALS_3, OUT_PTRS_3, 0, cart, cache);
    b.cols[180] = Col::U(AV::Num(P8::from_raw(1441792i32)));
    b.cols[207] = Col::U(AV::Num(P8::from_raw(6291456i32)));
    b.cols[199] = Col::U(AV::Bool(true));
    b.cols[201] = Col::U(AV::Num(P8::from_raw(1310720i32)));
    b.cols[87] = Col::N(Vec::new());
    b.cols[39] = Col::N(Vec::new());
    b.cols[193] = Col::U(AV::Bool(true));
    b.cols[194] = Col::U(AV::Num(P8::from_raw(4194304i32)));
    b.cols[183] = Col::U(AV::Num(P8::from_raw(1507328i32)));
    b.cols[189] = Col::U(AV::Bool(true));
    b.cols[191] = Col::U(AV::Num(P8::from_raw(1835008i32)));
    b.cols[84] = Col::N(Vec::new());
    b.cols[20] = Col::N(Vec::new());
    b.cols[185] = Col::U(AV::Bool(true));
    b.cols[187] = Col::U(AV::Num(P8::from_raw(1703936i32)));
    b.cols[175] = Col::U(AV::Bool(true));
    b.cols[41] = Col::U(AV::Bool(false));
    b.cols[42] = Col::U(AV::Bool(false));
    b.cols[49] = Col::U(AV::Num(P8::from_raw(327680i32)));
    b.cols[47] = Col::U(AV::Num(P8::from_raw(196608i32)));
    b.cols[48] = Col::U(AV::Num(P8::from_raw(262144i32)));
    b.cols[44] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[45] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[46] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[196] = Col::U(AV::Bool(true));
    b.cols[197] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[88] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[86] = Col::N(Vec::new());
    b.cols[240] = Col::U(AV::Bool(true));
    b.cols[241] = Col::N(Vec::new());
    b.cols[330] = Col::U(AV::Bool(false));
    b.cols[331] = Col::U(AV::Bool(false));
    b.cols[332] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[333] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[334] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[335] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[336] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[337] = Col::N(Vec::new());
    b.cols[250] = Col::U(AV::Bool(false));
    b.cols[338] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[339] = Col::N(Vec::new());
    b.cols[252] = Col::N(Vec::new());
    b.cols[253] = Col::N(Vec::new());
    b.cols[340] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[341] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[159] = Col::U(AV::Num(P8::from_raw(65536i32)));
    b.cols[256] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[257] = Col::N(Vec::new());
    b.cols[260] = Col::U(AV::Bool(true));
    b.cols[342] = Col::U(AV::Bool(false));
    b.cols[343] = Col::U(AV::Bool(false));
    b.cols[262] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[263] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[344] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[345] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[346] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[347] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[348] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[349] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[271] = Col::U(AV::Bool(true));
    b.cols[350] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[351] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[273] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[177] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[275] = Col::U(AV::Num(P8::from_raw(2621440i32)));
    b.cols[276] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[279] = Col::U(AV::Bool(true));
    b.cols[352] = Col::U(AV::Bool(false));
    b.cols[353] = Col::U(AV::Bool(false));
    b.cols[281] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[282] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[354] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[355] = Col::U(AV::Num(P8::from_raw(524288i32)));
    b.cols[356] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[357] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[358] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[359] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[290] = Col::U(AV::Bool(true));
    b.cols[360] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[361] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[292] = Col::U(AV::Num(P8::from_raw(1179648i32)));
    b.cols[294] = Col::U(AV::Num(P8::from_raw(6815744i32)));
    b.cols[295] = Col::U(AV::Num(P8::from_raw(7340032i32)));
    b.cols[43] = Col::U(AV::Bool(false));
    b.cols[161] = Col::U(AV::Num(P8::from_raw(131072i32)));
    b.cols[162] = Col::U(AV::Num(P8::from_raw(0i32)));
    b.cols[85] = Col::N(Vec::new());
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
        if let Col::N(v) = &mut acc.cols[87] { v.push(sh.c87.lane(i)); }
        if let Col::N(v) = &mut acc.cols[39] { v.push(sh.c39.lane(i)); }
        if let Col::N(v) = &mut acc.cols[84] { v.push(sh.c84.lane(i)); }
        if let Col::N(v) = &mut acc.cols[20] { v.push(sh.c20.lane(i)); }
        if let Col::N(v) = &mut acc.cols[86] { v.push(sh.c86.lane(i)); }
        if let Col::N(v) = &mut acc.cols[241] { v.push(sh.c241.lane(i)); }
        if let Col::N(v) = &mut acc.cols[337] { v.push(sh.c337.lane(i)); }
        if let Col::N(v) = &mut acc.cols[339] { v.push(sh.c339.lane(i)); }
        if let Col::N(v) = &mut acc.cols[252] { v.push(sh.c252.lane(i)); }
        if let Col::N(v) = &mut acc.cols[253] { v.push(sh.c253.lane(i)); }
        if let Col::N(v) = &mut acc.cols[257] { v.push(sh.c257.lane(i)); }
        if let Col::N(v) = &mut acc.cols[85] { v.push(sh.c85.lane(i)); }
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
    let r_c175: ZB = ZB { val: rin.c175, known: ALL };
    let r_c240: ZB = ZB { val: rin.c240, known: ALL };
    let r_c241: ZN = rin.c241;
    let r_c250: ZB = ZB { val: rin.c250, known: ALL };
    let r_c252: ZN = rin.c252;
    let r_c253: ZN = rin.c253;
    let r_c256: ZN = rin.c256;
    let r_c257: ZN = rin.c257;
    let r_c260: ZB = ZB { val: rin.c260, known: ALL };
    let r_c262: ZN = rin.c262;
    let r_c263: ZN = rin.c263;
    let r_c271: ZB = ZB { val: rin.c271, known: ALL };
    let r_c273: ZN = rin.c273;
    let r_c275: ZN = rin.c275;
    let r_c276: ZN = rin.c276;
    let r_c279: ZB = ZB { val: rin.c279, known: ALL };
    let r_c281: ZN = rin.c281;
    let r_c282: ZN = rin.c282;
    let r_c290: ZB = ZB { val: rin.c290, known: ALL };
    let r_c292: ZN = rin.c292;
    let r_c294: ZN = rin.c294;
    let r_c295: ZN = rin.c295;
    let r_c330: ZB = ZB { val: rin.c330, known: ALL };
    let r_c331: ZB = ZB { val: rin.c331, known: ALL };
    let r_c336: ZN = rin.c336;
    let r_c337: ZN = rin.c337;
    let r_c338: ZN = rin.c338;
    let r_c339: ZN = rin.c339;
    let r_c340: ZN = rin.c340;
    let r_c341: ZN = rin.c341;
    let r_c342: ZB = ZB { val: rin.c342, known: ALL };
    let r_c343: ZB = ZB { val: rin.c343, known: ALL };
    let r_c348: ZN = rin.c348;
    let r_c349: ZN = rin.c349;
    let r_c350: ZN = rin.c350;
    let r_c351: ZN = rin.c351;
    let r_c352: ZB = ZB { val: rin.c352, known: ALL };
    let r_c353: ZB = ZB { val: rin.c353, known: ALL };
    let r_c358: ZN = rin.c358;
    let r_c359: ZN = rin.c359;
    let r_c360: ZN = rin.c360;
    let r_c361: ZN = rin.c361;
    let n95: ZB = zb_not(r_c41);
    let n96: bool = P8::from_raw(0i32) == u.c335;
    let n97: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c336);
    let n98: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c338);
    let n99: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c262);
    let n100: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c263);
    let n101: bool = P8::from_raw(0i32) == u.c347;
    let n102: ZB = zb_not(r_c352);
    let n103: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c281);
    let n104: bool = P8::from_raw(524288i32) == u.c354;
    let n105: bool = P8::from_raw(0i32) == u.c356;
    let n106: bool = P8::from_raw(0i32) == u.c357;
    let n107: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c358);
    let n108: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c360);
    let n109: ZB = zb_not(r_c43);
    let n113: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c84);
    let n114: ZN = zn_rem(n113, zn_splat(P8::from_raw(1966080i32)));
    let n115: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n114);
    let n125: bool = P8::from_raw(0i32) == u.c346;
    let n126: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c348);
    let n127: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c349);
    let n128: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c282);
    let n129: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c359);
    let n130: ZB = zn_le(r_c20, zn_splat(P8::from_raw(0i32)));
    let n131: ZB = zb_not(r_c42);
    let n132: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c88);
    let n133: ZB = zb_not(r_c330);
    let n134: bool = P8::from_raw(524288i32) == u.c333;
    let n135: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c339);
    let n136: ZB = zb_not(r_c342);
    let n137: bool = P8::from_raw(524288i32) == u.c344;
    let n138: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c350);
    let n139: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c86);
    let n144: ZB = zb_not(r_c331);
    let n145: bool = P8::from_raw(524288i32) == u.c332;
    let n146: bool = P8::from_raw(0i32) == u.c334;
    let n147: ZB = zb_not(r_c250);
    let n148: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c253);
    let n149: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c340);
    let n150: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c341);
    let n151: ZB = zn_eq(zn_splat(P8::from_raw(524288i32)), r_c256);
    let n152: ZB = zb_not(r_c343);
    let n153: bool = P8::from_raw(524288i32) == u.c345;
    let n154: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c351);
    let n155: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c273);
    let n156: ZB = zn_eq(zn_splat(P8::from_raw(2621440i32)), r_c275);
    let n157: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c276);
    let n158: ZB = zb_not(r_c353);
    let n159: bool = P8::from_raw(524288i32) == u.c355;
    let n160: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), r_c361);
    let n161: ZB = zn_eq(zn_splat(P8::from_raw(1179648i32)), r_c292);
    let n162: ZB = zn_eq(zn_splat(P8::from_raw(6815744i32)), r_c294);
    let n163: ZB = zn_eq(zn_splat(P8::from_raw(7340032i32)), r_c295);
    let n164: ZB = zb_not(r_c38);
    let n165: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c85);
    let n166: ZN = zn_rem(n165, zn_splat(P8::from_raw(3932160i32)));
    let n167: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n166);
    let n168: ZN = zsel_n(n167, n139, r_c86);
    let n169: ZN = zsel_n(n115, n168, r_c86);
    let n170: ZN = zsel_n(n115, n166, r_c85);
    let n178: ZB = zb_not(n148);
    let n179: ZB = zn_eq(zn_splat(P8::from_raw(65536i32)), r_c253);
    let n180: ZB = zb_not(n179);
    let n181: ZB = zn_eq(zn_splat(P8::from_raw(131072i32)), r_c253);
    let n182: ZN = zn_sub(r_c241, zn_splat(P8::from_raw(65536i32)));
    let n183: ZB = zn_lt(n182, zn_splat(P8::from_raw(0i32)));
    let n184: ZB = zn_gt(r_c20, zn_splat(P8::from_raw(0i32)));
    let n185: ZB = zb_not(n135);
    let n186: ZB = zb_and(n130, n178);
    let n187: ZB = zb_and(n180, n186);
    let n188: ZB = zb_and(n181, n187);
    let n189: ZB = zb_and(n183, n188);
    let n231: ZN = zn_add(r_c337, r_c339);
    let n232: ZN = zn_add(zn_splat(P8::from_raw(32768i32)), n231);
    let n233: ZN = zn_flr(n232);
    let n234: ZN = zn_add(r_c257, n233);
    let n235: ZN = zsel_n(n185, n234, r_c257);
    let n236: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n235);
    let n237: ZN = zn_div(n236, zn_splat(P8::from_raw(524288i32)));
    let n238: ZN = zn_flr(n237);
    let n239: ZN = zn_max(zn_splat(P8::from_raw(0i32)), n238);
    let n240: ZN = zn_add(zn_splat(P8::from_raw(327680i32)), n236);
    let n241: ZN = zn_sub(n240, zn_splat(P8::from_raw(65536i32)));
    let n242: ZN = zn_div(n241, zn_splat(P8::from_raw(524288i32)));
    let n243: ZN = zn_min(zn_splat(P8::from_raw(983040i32)), n242);
    let n244: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n239);
    let n245: ZB = zn_le(n244, n243);
    let n246: ZB = zn_gt(n244, n243);
    let n247: ZB = zb_and(n189, n245);
    let n248: ZB = zb_and(n189, n246);
    let n249: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n244);
    let n250: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n249);
    let n251: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n250);
    let n252: ZB = zb_not(n251);
    let n253: ZB = zb_and(n247, n251);
    let n254: ZB = zb_and(n247, n252);
    let n255: ZN = zn_rem(n241, zn_splat(P8::from_raw(524288i32)));
    let n256: ZB = zn_ge(n255, zn_splat(P8::from_raw(393216i32)));
    let n257: ZB = zn_lt(n255, zn_splat(P8::from_raw(393216i32)));
    let n258: ZB = zb_and(n253, n257);
    let n259: ZB = zb_and(n253, n256);
    let n260: ZN = zn_mul(n244, zn_splat(P8::from_raw(524288i32)));
    let n261: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n260);
    let n262: ZB = zn_eq(n240, n261);
    let n263: ZB = zb_or(n258, n259);
    let n264: ZB = zb_or(n256, n262);
    let n265: ZB = zb_or(n254, n263);
    let n266: ZB = zb_and(n251, n264);
    let n267: ZB = zb_not(n266);
    let n268: ZB = zb_and(n265, n266);
    let n269: ZB = zb_and(n265, n267);
    let n270: ZB = zb_or(n268, n269);
    let n271: ZB = zb_and(n267, n270);
    let n272: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n250);
    let n273: ZB = zb_not(n272);
    let n274: ZB = zb_and(n271, n272);
    let n275: ZB = zb_and(n271, n273);
    let n276: ZN = zn_rem(n236, zn_splat(P8::from_raw(524288i32)));
    let n277: ZB = zn_le(n276, zn_splat(P8::from_raw(131072i32)));
    let n278: ZB = zb_or(n274, n275);
    let n279: ZB = zb_and(n272, n277);
    let n280: ZB = zb_not(n279);
    let n281: ZB = zb_and(n278, n279);
    let n282: ZB = zb_and(n278, n280);
    let n283: ZB = zb_or(n281, n282);
    let n284: ZB = zb_and(n280, n283);
    let n285: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n250);
    let n286: ZB = zb_not(n285);
    let n287: ZB = zb_and(n284, n285);
    let n288: ZB = zb_and(n284, n286);
    let n289: ZB = zb_or(n287, n288);
    let n290: ZB = zb_and(n285, n289);
    let n291: ZB = zb_and(n286, n289);
    let n292: ZB = zb_or(n290, n291);
    let n293: ZB = zb_and(n286, n292);
    let n294: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n250);
    let n295: ZB = zb_not(n294);
    let n296: ZB = zb_and(n293, n294);
    let n297: ZB = zb_and(n293, n295);
    let n298: ZB = zb_or(n296, n297);
    let n299: ZB = zb_and(n294, n298);
    let n300: ZB = zb_and(n295, n298);
    let n301: ZB = zb_or(n299, n300);
    let n302: ZB = zb_and(n295, n301);
    let n303: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n239);
    let n304: ZB = zn_le(n303, n243);
    let n305: ZB = zn_gt(n303, n243);
    let n306: ZB = zb_and(n302, n304);
    let n307: ZB = zb_and(n302, n305);
    let n308: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n303);
    let n309: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n308);
    let n310: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n309);
    let n311: ZB = zb_not(n310);
    let n312: ZB = zb_and(n306, n310);
    let n313: ZB = zb_and(n306, n311);
    let n314: ZB = zb_and(n257, n312);
    let n315: ZB = zb_and(n256, n312);
    let n316: ZN = zn_mul(n303, zn_splat(P8::from_raw(524288i32)));
    let n317: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n316);
    let n318: ZB = zn_eq(n240, n317);
    let n319: ZB = zb_or(n314, n315);
    let n320: ZB = zb_or(n256, n318);
    let n321: ZB = zb_or(n313, n319);
    let n322: ZB = zb_and(n310, n320);
    let n323: ZB = zb_not(n322);
    let n324: ZB = zb_and(n321, n322);
    let n325: ZB = zb_and(n321, n323);
    let n326: ZB = zb_or(n324, n325);
    let n327: ZB = zb_and(n323, n326);
    let n328: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n309);
    let n329: ZB = zb_not(n328);
    let n330: ZB = zb_and(n327, n328);
    let n331: ZB = zb_and(n327, n329);
    let n332: ZB = zb_or(n330, n331);
    let n333: ZB = zb_and(n277, n328);
    let n334: ZB = zb_not(n333);
    let n335: ZB = zb_and(n332, n333);
    let n336: ZB = zb_and(n332, n334);
    let n337: ZB = zb_or(n335, n336);
    let n338: ZB = zb_and(n334, n337);
    let n339: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n309);
    let n340: ZB = zb_not(n339);
    let n341: ZB = zb_and(n338, n339);
    let n342: ZB = zb_and(n338, n340);
    let n343: ZB = zb_or(n341, n342);
    let n344: ZB = zb_and(n339, n343);
    let n345: ZB = zb_and(n340, n343);
    let n346: ZB = zb_or(n344, n345);
    let n347: ZB = zb_and(n340, n346);
    let n348: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n309);
    let n349: ZB = zb_not(n348);
    let n350: ZB = zb_and(n347, n348);
    let n351: ZB = zb_and(n347, n349);
    let n352: ZB = zb_or(n350, n351);
    let n353: ZB = zb_and(n348, n352);
    let n354: ZB = zb_and(n349, n352);
    let n355: ZB = zb_or(n353, n354);
    let n356: ZB = zb_and(n349, n355);
    let n357: ZN = zn_add(zn_splat(P8::from_raw(131072i32)), n239);
    let n358: ZB = zn_le(n357, n243);
    let n359: ZB = zn_gt(n357, n243);
    let n360: ZB = zb_and(n356, n358);
    let n361: ZB = zb_and(n356, n359);
    let n362: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n357);
    let n363: ZN = zn_mget(g.cart, zn_splat(P8::from_raw(2162688i32)), n362);
    let n364: ZB = zn_eq(zn_splat(P8::from_raw(1114112i32)), n363);
    let n365: ZB = zb_not(n364);
    let n366: ZB = zb_and(n360, n364);
    let n367: ZB = zb_and(n360, n365);
    let n368: ZB = zb_and(n257, n366);
    let n369: ZB = zb_and(n256, n366);
    let n370: ZN = zn_mul(n357, zn_splat(P8::from_raw(524288i32)));
    let n371: ZN = zn_add(zn_splat(P8::from_raw(524288i32)), n370);
    let n372: ZB = zn_eq(n240, n371);
    let n373: ZB = zb_or(n368, n369);
    let n374: ZB = zb_or(n256, n372);
    let n375: ZB = zb_or(n367, n373);
    let n376: ZB = zb_and(n364, n374);
    let n377: ZB = zb_not(n376);
    let n378: ZB = zb_and(n375, n376);
    let n379: ZB = zb_and(n375, n377);
    let n380: ZB = zb_or(n378, n379);
    let n381: ZB = zb_and(n377, n380);
    let n382: ZB = zn_eq(zn_splat(P8::from_raw(1769472i32)), n363);
    let n383: ZB = zb_not(n382);
    let n384: ZB = zb_and(n381, n382);
    let n385: ZB = zb_and(n381, n383);
    let n386: ZB = zb_or(n384, n385);
    let n387: ZB = zb_and(n277, n382);
    let n388: ZB = zb_not(n387);
    let n389: ZB = zb_and(n386, n387);
    let n390: ZB = zb_and(n386, n388);
    let n391: ZB = zb_or(n389, n390);
    let n392: ZB = zb_and(n388, n391);
    let n393: ZB = zn_eq(zn_splat(P8::from_raw(2818048i32)), n363);
    let n394: ZB = zb_not(n393);
    let n395: ZB = zb_and(n392, n393);
    let n396: ZB = zb_and(n392, n394);
    let n397: ZB = zb_or(n395, n396);
    let n398: ZB = zb_and(n393, n397);
    let n399: ZB = zb_and(n394, n397);
    let n400: ZB = zb_or(n398, n399);
    let n401: ZB = zb_and(n394, n400);
    let n402: ZB = zn_eq(zn_splat(P8::from_raw(3866624i32)), n363);
    let n403: ZB = zb_not(n402);
    let n404: ZB = zb_and(n401, n402);
    let n405: ZB = zb_and(n401, n403);
    let n406: ZB = zb_or(n404, n405);
    let n407: ZB = zb_and(n402, n406);
    let n408: ZB = zb_and(n403, n406);
    let n409: ZB = zb_or(n407, n408);
    let n410: ZB = zb_and(n403, n409);
    let n411: ZN = zn_add(zn_splat(P8::from_raw(196608i32)), n239);
    let n412: ZB = zn_gt(n411, n243);
    let n413: ZB = zb_or(n361, n410);
    let n414: ZB = zb_or(n359, n412);
    let n415: ZB = zb_or(n307, n413);
    let n416: ZB = zb_or(n305, n414);
    let n417: ZB = zb_or(n248, n415);
    let n418: ZB = zb_or(n246, n416);
    let n419: ZB = zn_le(n235, zn_splat(P8::from_raw(8388608i32)));
    let n420: ZB = zb_and(n417, n419);
    let n421: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n236);
    let n422: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(589824i32)), n421, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n423: ZB = zb_not(n422);
    let n424: ZB = zb_and(n420, n423);
    let n425: ZB = zb_and(n420, n422);
    let n426: ZB = zb_or(n424, n425);
    let n427: ZB = zb_and(n423, n426);
    let n428: ZB = zb_and(n422, n426);
    let n429: ZB = zb_or(n427, n428);
    let n430: ZB = zb_and(n422, n429);
    let n431: ZB = zb_and(n423, n429);
    let n432: ZN = zsel_n(n422, zn_splat(P8::from_raw(393216i32)), zn_splat(P8::from_raw(0i32)));
    let n433: ZB = zb_or(n430, n431);
    let n434: ZB = zb_and(n423, n433);
    let n435: ZB = zb_and(n422, n433);
    let n436: ZN = zsel_n(n423, zn_splat(P8::from_raw(26214i32)), zn_splat(P8::from_raw(39321i32)));
    let n437: ZB = zb_or(n434, n435);
    let n438: ZN = zn_sub(zn_splat(P8::from_raw(0i32)), n436);
    let n439: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n436);
    let n440: ZN = zn_add(zn_splat(P8::from_raw(0i32)), n236);
    let n441: ZB = zb_and(n423, n437);
    let n442: ZB = zb_and(n422, n437);
    let n443: ZN = zsel_n(n423, zn_splat(P8::from_raw(6881i32)), zn_splat(P8::from_raw(0i32)));
    let n444: ZB = zb_or(n441, n442);
    let n445: ZB = zn_gt(n432, zn_splat(P8::from_raw(0i32)));
    let n446: ZB = zn_le(n432, zn_splat(P8::from_raw(0i32)));
    let n447: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(393216i32)), n440, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n448: ZB = zb_not(n447);
    let n449: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(786432i32)), n440, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n450: ZB = zb_not(n449);
    let n451: ZN = zsel_n(n449, zn_splat(P8::from_raw(65536i32)), zn_splat(P8::from_raw(0i32)));
    let n452: ZN = zsel_n(n447, zn_splat(P8::from_raw(-65536i32)), n451);
    let n453: ZB = zn_eq(zn_splat(P8::from_raw(0i32)), n452);
    let n454: ZB = zb_not(n453);
    let n455: ZN = zn_neg(n452);
    let n456: ZN = zn_mul(n455, zn_splat(P8::from_raw(131072i32)));
    let n457: ZN = zsel_n(n454, n456, zn_splat(P8::from_raw(0i32)));
    let n458: ZN = zsel_n(n454, zn_splat(P8::from_raw(-131072i32)), n443);
    let n459: ZN = zsel_n(n445, zn_splat(P8::from_raw(0i32)), n432);
    let n460: ZN = zsel_n(n445, zn_splat(P8::from_raw(0i32)), n457);
    let n461: ZN = zsel_n(n445, zn_splat(P8::from_raw(-131072i32)), n458);
    let n462: ZB = zn_lt(n235, zn_splat(P8::from_raw(-262144i32)));
    let n463: ZB = zn_ge(n235, zn_splat(P8::from_raw(-262144i32)));
    let n464: ZB = zb_and(n444, n462);
    let n465: ZB = zb_and(n444, n463);
    let n466: ZB = zb_or(n464, n465);
    let n472: ZN = zn_max(zn_splat(P8::from_raw(-65536i32)), n438);
    let n473: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(524288i32)), n440, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n474: ZB = zb_not(n473);
    let n475: ZB = zb_and(n437, n474);
    let n476: ZB = zb_and(n437, n473);
    let n477: ZB = zb_or(n475, n476);
    let n478: ZB = zb_and(n474, n477);
    let n479: ZB = zb_and(n473, n477);
    let n480: ZB = zb_or(n478, n479);
    let n481: ZB = zb_and(n473, n480);
    let n482: ZB = zb_and(n474, n480);
    let n483: ZB = zb_or(n481, n482);
    let n484: ZB = zb_and(n473, n483);
    let n485: ZB = zb_and(n474, n483);
    let n486: ZB = zb_or(n484, n485);
    let n487: ZB = zb_and(n423, n486);
    let n488: ZB = zb_and(n422, n486);
    let n489: ZB = zb_or(n487, n488);
    let n490: ZN = zsel_n(n454, n456, n472);
    let n491: ZN = zsel_n(n445, n472, n490);
    let n492: ZB = zb_and(n462, n489);
    let n493: ZB = zb_and(n463, n489);
    let n494: ZB = zb_or(n492, n493);
    let n497: ZN = zn_min(zn_splat(P8::from_raw(65536i32)), n439);
    let n498: ZB = zn_tile_flag_at(g.cache, g.cart, zn_splat(P8::from_raw(655360i32)), n440, P8::from_raw(393216i32), P8::from_raw(327680i32), P8::from_raw(0i32));
    let n499: ZB = zb_not(n498);
    let n500: ZB = zb_and(n437, n499);
    let n501: ZB = zb_and(n437, n498);
    let n502: ZB = zb_or(n500, n501);
    let n503: ZB = zb_and(n499, n502);
    let n504: ZB = zb_and(n498, n502);
    let n505: ZB = zb_or(n503, n504);
    let n506: ZB = zb_and(n498, n505);
    let n507: ZB = zb_and(n499, n505);
    let n508: ZB = zb_or(n506, n507);
    let n509: ZB = zb_and(n498, n508);
    let n510: ZB = zb_and(n499, n508);
    let n511: ZB = zb_or(n509, n510);
    let n512: ZB = zb_and(n423, n511);
    let n513: ZB = zb_and(n422, n511);
    let n514: ZB = zb_or(n512, n513);
    let n515: ZN = zsel_n(n454, n456, n497);
    let n516: ZN = zsel_n(n445, n497, n515);
    let n517: ZB = zb_and(n462, n514);
    let n518: ZB = zb_and(n463, n514);
    let n519: ZB = zb_or(n517, n518);
    let n522: ZB = zb_and(n444, n445);
    let n523: ZB = zb_and(n444, n446);
    let n524: ZB = zb_and(n448, n523);
    let n525: ZB = zb_and(n447, n523);
    let n526: ZB = zb_or(n524, n525);
    let n527: ZB = zb_and(n448, n526);
    let n528: ZB = zb_and(n447, n526);
    let n529: ZB = zb_or(n527, n528);
    let n530: ZB = zb_and(n447, n529);
    let n531: ZB = zb_and(n448, n529);
    let n532: ZB = zb_and(n450, n531);
    let n533: ZB = zb_and(n449, n531);
    let n534: ZB = zb_or(n532, n533);
    let n535: ZB = zb_and(n450, n534);
    let n536: ZB = zb_and(n449, n534);
    let n537: ZB = zb_or(n535, n536);
    let n538: ZB = zb_and(n449, n537);
    let n539: ZB = zb_and(n450, n537);
    let n540: ZB = zb_or(n538, n539);
    let n541: ZB = zb_or(n530, n540);
    let n542: ZB = zb_and(n454, n541);
    let n543: ZB = zb_and(n453, n541);
    let n544: ZB = zb_or(n542, n543);
    let n545: ZB = zb_or(n522, n544);
    let n546: ZB = zb_and(n462, n545);
    let n547: ZB = zb_and(n463, n545);
    let n548: ZB = zb_or(n546, n547);
    let n551: ZB = zb_and(n445, n489);
    let n552: ZB = zb_and(n446, n489);
    let n553: ZB = zb_and(n448, n552);
    let n554: ZB = zb_and(n447, n552);
    let n555: ZB = zb_or(n553, n554);
    let n556: ZB = zb_and(n448, n555);
    let n557: ZB = zb_and(n447, n555);
    let n558: ZB = zb_or(n556, n557);
    let n559: ZB = zb_and(n447, n558);
    let n560: ZB = zb_and(n448, n558);
    let n561: ZB = zb_and(n450, n560);
    let n562: ZB = zb_and(n449, n560);
    let n563: ZB = zb_or(n561, n562);
    let n564: ZB = zb_and(n450, n563);
    let n565: ZB = zb_and(n449, n563);
    let n566: ZB = zb_or(n564, n565);
    let n567: ZB = zb_and(n449, n566);
    let n568: ZB = zb_and(n450, n566);
    let n569: ZB = zb_or(n567, n568);
    let n570: ZB = zb_or(n559, n569);
    let n571: ZB = zb_and(n454, n570);
    let n572: ZB = zb_and(n453, n570);
    let n573: ZB = zb_or(n571, n572);
    let n574: ZB = zb_or(n551, n573);
    let n575: ZB = zb_and(n462, n574);
    let n576: ZB = zb_and(n463, n574);
    let n577: ZB = zb_or(n575, n576);
    let n580: ZB = zb_and(n445, n514);
    let n581: ZB = zb_and(n446, n514);
    let n582: ZB = zb_and(n448, n581);
    let n583: ZB = zb_and(n447, n581);
    let n584: ZB = zb_or(n582, n583);
    let n585: ZB = zb_and(n448, n584);
    let n586: ZB = zb_and(n447, n584);
    let n587: ZB = zb_or(n585, n586);
    let n588: ZB = zb_and(n447, n587);
    let n589: ZB = zb_and(n448, n587);
    let n590: ZB = zb_and(n450, n589);
    let n591: ZB = zb_and(n449, n589);
    let n592: ZB = zb_or(n590, n591);
    let n593: ZB = zb_and(n450, n592);
    let n594: ZB = zb_and(n449, n592);
    let n595: ZB = zb_or(n593, n594);
    let n596: ZB = zb_and(n449, n595);
    let n597: ZB = zb_and(n450, n595);
    let n598: ZB = zb_or(n596, n597);
    let n599: ZB = zb_or(n588, n598);
    let n600: ZB = zb_and(n454, n599);
    let n601: ZB = zb_and(n453, n599);
    let n602: ZB = zb_or(n600, n601);
    let n603: ZB = zb_or(n580, n602);
    let n604: ZB = zb_and(n462, n603);
    let n605: ZB = zb_and(n463, n603);
    let n606: ZB = zb_or(n604, n605);
    let n610: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), r_c87);
    let n611: ZN = zn_add(zn_splat(P8::from_raw(65536i32)), n610);
    let n612: ZB = zb_and(n266, n270);
    let n613: ZB = zb_and(n279, n283);
    let n614: ZB = zb_and(n285, n292);
    let n615: ZB = zb_and(n294, n301);
    let n616: ZB = zb_or(n614, n615);
    let n617: ZB = zb_or(n613, n616);
    let n618: ZB = zb_or(n612, n617);
    let n619: ZB = zb_and(n322, n326);
    let n620: ZB = zb_and(n333, n337);
    let n621: ZB = zb_and(n339, n346);
    let n622: ZB = zb_and(n348, n355);
    let n623: ZB = zb_or(n621, n622);
    let n624: ZB = zb_or(n620, n623);
    let n625: ZB = zb_or(n619, n624);
    let n626: ZB = zb_and(n376, n380);
    let n627: ZB = zb_and(n387, n391);
    let n628: ZB = zb_and(n393, n400);
    let n629: ZB = zb_and(n402, n409);
    let n630: ZB = zb_or(n628, n629);
    let n631: ZB = zb_or(n627, n630);
    let n632: ZB = zb_or(n626, n631);
    let n633: ZB = zb_or(n625, n632);
    let n634: ZB = zb_or(n618, n633);
    let n635: ZB = zn_gt(n235, zn_splat(P8::from_raw(8388608i32)));
    let n636: ZB = zb_and(n634, n635);
    let n637: ZB = zb_and(n419, n634);
    let n638: ZN = zsel_n(n635, n611, n610);
    let n639: ZB = zb_or(n636, n637);
    let n640: ZB = zb_and(n417, n635);
    let n641: ZN = zsel_n(n639, n638, n610);
    let n642: ZB = zb_or(n639, n640);
    let n643: ZB = zb_or(n418, n639);
    let n644: ZB = zb_and(n423, n642);
    let n645: ZB = zb_and(n422, n642);
    let n646: ZB = zb_or(n644, n645);
    let n647: ZB = zb_and(n423, n646);
    let n648: ZB = zb_and(n422, n646);
    let n649: ZB = zb_or(n647, n648);
    let n650: ZB = zb_and(n422, n649);
    let n651: ZB = zb_and(n423, n649);
    let n652: ZB = zb_or(n650, n651);
    let n653: ZB = zb_and(n423, n652);
    let n654: ZB = zb_and(n422, n652);
    let n655: ZB = zb_or(n653, n654);
    let n656: ZB = zb_and(n423, n655);
    let n657: ZB = zb_and(n422, n655);
    let n658: ZB = zb_or(n656, n657);
    let n659: ZB = zb_and(n462, n658);
    let n660: ZB = zb_and(n463, n658);
    let n661: ZB = zb_or(n659, n660);
    let n665: ZB = zb_and(n474, n655);
    let n666: ZB = zb_and(n473, n655);
    let n667: ZB = zb_or(n665, n666);
    let n668: ZB = zb_and(n474, n667);
    let n669: ZB = zb_and(n473, n667);
    let n670: ZB = zb_or(n668, n669);
    let n671: ZB = zb_and(n473, n670);
    let n672: ZB = zb_and(n474, n670);
    let n673: ZB = zb_or(n671, n672);
    let n674: ZB = zb_and(n473, n673);
    let n675: ZB = zb_and(n474, n673);
    let n676: ZB = zb_or(n674, n675);
    let n677: ZB = zb_and(n423, n676);
    let n678: ZB = zb_and(n422, n676);
    let n679: ZB = zb_or(n677, n678);
    let n680: ZB = zb_and(n462, n679);
    let n681: ZB = zb_and(n463, n679);
    let n682: ZB = zb_or(n680, n681);
    let n685: ZB = zb_and(n499, n655);
    let n686: ZB = zb_and(n498, n655);
    let n687: ZB = zb_or(n685, n686);
    let n688: ZB = zb_and(n499, n687);
    let n689: ZB = zb_and(n498, n687);
    let n690: ZB = zb_or(n688, n689);
    let n691: ZB = zb_and(n498, n690);
    let n692: ZB = zb_and(n499, n690);
    let n693: ZB = zb_or(n691, n692);
    let n694: ZB = zb_and(n498, n693);
    let n695: ZB = zb_and(n499, n693);
    let n696: ZB = zb_or(n694, n695);
    let n697: ZB = zb_and(n423, n696);
    let n698: ZB = zb_and(n422, n696);
    let n699: ZB = zb_or(n697, n698);
    let n700: ZB = zb_and(n462, n699);
    let n701: ZB = zb_and(n463, n699);
    let n702: ZB = zb_or(n700, n701);
    let n705: ZB = zb_and(n445, n658);
    let n706: ZB = zb_and(n446, n658);
    let n707: ZB = zb_and(n448, n706);
    let n708: ZB = zb_and(n447, n706);
    let n709: ZB = zb_or(n707, n708);
    let n710: ZB = zb_and(n448, n709);
    let n711: ZB = zb_and(n447, n709);
    let n712: ZB = zb_or(n710, n711);
    let n713: ZB = zb_and(n447, n712);
    let n714: ZB = zb_and(n448, n712);
    let n715: ZB = zb_and(n450, n714);
    let n716: ZB = zb_and(n449, n714);
    let n717: ZB = zb_or(n715, n716);
    let n718: ZB = zb_and(n450, n717);
    let n719: ZB = zb_and(n449, n717);
    let n720: ZB = zb_or(n718, n719);
    let n721: ZB = zb_and(n449, n720);
    let n722: ZB = zb_and(n450, n720);
    let n723: ZB = zb_or(n721, n722);
    let n724: ZB = zb_or(n713, n723);
    let n725: ZB = zb_and(n454, n724);
    let n726: ZB = zb_and(n453, n724);
    let n727: ZB = zb_or(n725, n726);
    let n728: ZB = zb_or(n705, n727);
    let n729: ZB = zb_and(n462, n728);
    let n730: ZB = zb_and(n463, n728);
    let n731: ZB = zb_or(n729, n730);
    let n734: ZB = zb_and(n445, n679);
    let n735: ZB = zb_and(n446, n679);
    let n736: ZB = zb_and(n448, n735);
    let n737: ZB = zb_and(n447, n735);
    let n738: ZB = zb_or(n736, n737);
    let n739: ZB = zb_and(n448, n738);
    let n740: ZB = zb_and(n447, n738);
    let n741: ZB = zb_or(n739, n740);
    let n742: ZB = zb_and(n447, n741);
    let n743: ZB = zb_and(n448, n741);
    let n744: ZB = zb_and(n450, n743);
    let n745: ZB = zb_and(n449, n743);
    let n746: ZB = zb_or(n744, n745);
    let n747: ZB = zb_and(n450, n746);
    let n748: ZB = zb_and(n449, n746);
    let n749: ZB = zb_or(n747, n748);
    let n750: ZB = zb_and(n449, n749);
    let n751: ZB = zb_and(n450, n749);
    let n752: ZB = zb_or(n750, n751);
    let n753: ZB = zb_or(n742, n752);
    let n754: ZB = zb_and(n454, n753);
    let n755: ZB = zb_and(n453, n753);
    let n756: ZB = zb_or(n754, n755);
    let n757: ZB = zb_or(n734, n756);
    let n758: ZB = zb_and(n462, n757);
    let n759: ZB = zb_and(n463, n757);
    let n760: ZB = zb_or(n758, n759);
    let n763: ZB = zb_and(n445, n699);
    let n764: ZB = zb_and(n446, n699);
    let n765: ZB = zb_and(n448, n764);
    let n766: ZB = zb_and(n447, n764);
    let n767: ZB = zb_or(n765, n766);
    let n768: ZB = zb_and(n448, n767);
    let n769: ZB = zb_and(n447, n767);
    let n770: ZB = zb_or(n768, n769);
    let n771: ZB = zb_and(n447, n770);
    let n772: ZB = zb_and(n448, n770);
    let n773: ZB = zb_and(n450, n772);
    let n774: ZB = zb_and(n449, n772);
    let n775: ZB = zb_or(n773, n774);
    let n776: ZB = zb_and(n450, n775);
    let n777: ZB = zb_and(n449, n775);
    let n778: ZB = zb_or(n776, n777);
    let n779: ZB = zb_and(n449, n778);
    let n780: ZB = zb_and(n450, n778);
    let n781: ZB = zb_or(n779, n780);
    let n782: ZB = zb_or(n771, n781);
    let n783: ZB = zb_and(n454, n782);
    let n784: ZB = zb_and(n453, n782);
    let n785: ZB = zb_or(n783, n784);
    let n786: ZB = zb_or(n763, n785);
    let n787: ZB = zb_and(n462, n786);
    let n788: ZB = zb_and(n463, n786);
    let n789: ZB = zb_or(n787, n788);
    let n798: ZB = zb_and(n462, n466);
    let n799: ZB = zb_and(n462, n661);
    let n800: ZN = zsel_n(n798, r_c87, n641);
    let n801: ZN = zsel_n(n798, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n802: ZB = zb_not(n798);
    let n803: ZB = zb_or(n798, n799);
    let n804: ZB = zsel_b(n798, n418, n643);
    let n807: ZB = zb_and(n462, n494);
    let n808: ZB = zb_and(n462, n682);
    let n809: ZN = zsel_n(n807, r_c87, n641);
    let n810: ZN = zsel_n(n807, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n811: ZB = zb_not(n807);
    let n812: ZB = zb_or(n807, n808);
    let n813: ZB = zsel_b(n807, n418, n643);
    let n816: ZB = zb_and(n462, n519);
    let n817: ZB = zb_and(n462, n702);
    let n818: ZN = zsel_n(n816, r_c87, n641);
    let n819: ZN = zsel_n(n816, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n820: ZB = zb_not(n816);
    let n821: ZB = zb_or(n816, n817);
    let n822: ZB = zsel_b(n816, n418, n643);
    let n825: ZB = zb_and(n462, n548);
    let n826: ZB = zb_and(n462, n731);
    let n827: ZN = zsel_n(n825, r_c87, n641);
    let n828: ZN = zsel_n(n825, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n829: ZB = zb_not(n825);
    let n830: ZB = zb_or(n825, n826);
    let n831: ZB = zsel_b(n825, n418, n643);
    let n834: ZB = zb_and(n462, n577);
    let n835: ZB = zb_and(n462, n760);
    let n836: ZN = zsel_n(n834, r_c87, n641);
    let n837: ZN = zsel_n(n834, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n838: ZB = zb_not(n834);
    let n839: ZB = zb_or(n834, n835);
    let n840: ZB = zsel_b(n834, n418, n643);
    let n843: ZB = zb_and(n462, n606);
    let n844: ZB = zb_and(n462, n789);
    let n845: ZN = zsel_n(n843, r_c87, n641);
    let n846: ZN = zsel_n(n843, r_c39, zn_splat(P8::from_raw(983040i32)));
    let n847: ZB = zb_not(n843);
    let n848: ZB = zb_or(n843, n844);
    let n849: ZB = zsel_b(n843, n418, n643);
    let n853: ZN = zn_sub(r_c20, zn_splat(P8::from_raw(65536i32)));
    let n854: ZN = zsel_n(n184, n853, r_c20);
    let n855: ZB = zn_gt(n854, zn_splat(P8::from_raw(0i32)));
    let n856: ZB = zn_le(n854, zn_splat(P8::from_raw(0i32)));
    let n857: ZB = zb_not(n181);
    let n858: ZB = zn_ge(n182, zn_splat(P8::from_raw(0i32)));
    let n859: ZN = zsel_n(n181, n182, r_c241);
    let n860: ZB = zb_and(n130, n148);
    let n861: ZB = zb_and(n179, n186);
    let n862: ZN = zn_add(r_c339, zn_splat(P8::from_raw(32768i32)));
    let n863: ZB = zn_gt(n862, zn_splat(P8::from_raw(0i32)));
    let n864: ZB = zn_le(n862, zn_splat(P8::from_raw(0i32)));
    let n865: ZB = zb_and(n861, n863);
    let n866: ZB = zb_and(n861, n864);
    let n867: ZB = zn_gt(r_c241, zn_splat(P8::from_raw(0i32)));
    let n868: ZB = zb_or(n865, n866);
    let n869: ZB = zb_and(n863, n867);
    let n870: ZB = zb_not(n869);
    let n871: ZB = zb_and(n868, n869);
    let n872: ZB = zb_and(n868, n870);
    let n873: ZN = zsel_n(n869, n182, r_c241);
    let n874: ZN = zsel_n(n869, zn_splat(P8::from_raw(0i32)), n862);
    let n875: ZB = zb_or(n871, n872);
    let n876: ZB = zn_gt(n874, zn_splat(P8::from_raw(0i32)));
    let n877: ZB = zn_le(n874, zn_splat(P8::from_raw(0i32)));
    let n878: ZB = zb_and(n875, n876);
    let n879: ZB = zb_and(n875, n877);
    let n880: ZB = zb_or(n878, n879);
    let n881: ZB = zb_and(n187, n857);
    let n882: ZB = zb_and(n188, n858);
    let n883: ZN = zsel_n(n181, zn_splat(P8::from_raw(393216i32)), r_c252);
    let n884: ZB = zb_or(n881, n882);
    let n885: ZN = zsel_n(n179, r_c252, n883);
    let n886: ZN = zsel_n(n148, r_c252, n885);
    let n887: ZN = zsel_n(n184, r_c252, n886);
    let n888: ZN = zn_sub(n232, zn_splat(P8::from_raw(32768i32)));
    let n889: ZN = zn_sub(n888, n233);
    let n890: ZN = zsel_n(n185, n889, r_c337);
    let n891: ZB = zn_lt(n235, zn_splat(P8::from_raw(7864320i32)));
    let n892: ZB = zn_ge(n235, zn_splat(P8::from_raw(7864320i32)));
    let n893: ZB = zb_and(n860, n891);
    let n894: ZB = zb_and(n860, n892);
    let n895: ZN = zsel_n(n891, zn_splat(P8::from_raw(196608i32)), r_c241);
    let n896: ZN = zsel_n(n891, zn_splat(P8::from_raw(65536i32)), r_c253);
    let n897: ZB = zb_or(n893, n894);
    let n898: ZB = zn_gt(n235, zn_splat(P8::from_raw(6815744i32)));
    let n899: ZB = zb_and(n876, n898);
    let n900: ZB = zb_not(n899);
    let n901: ZB = zb_and(n880, n899);
    let n902: ZB = zb_and(n880, n900);
    let n903: ZN = zsel_n(n899, zn_splat(P8::from_raw(327680i32)), n873);
    let n904: ZN = zsel_n(n899, zn_splat(P8::from_raw(131072i32)), r_c253);
    let n905: ZN = zsel_n(n899, zn_splat(P8::from_raw(6815744i32)), n235);
    let n906: ZN = zsel_n(n899, zn_splat(P8::from_raw(0i32)), n874);
    let n907: ZB = zb_or(n901, n902);
    let n908: ZN = zsel_n(n179, n903, n859);
    let n909: ZN = zsel_n(n179, n904, r_c253);
    let n910: ZN = zsel_n(n179, n905, n235);
    let n911: ZN = zsel_n(n179, n906, r_c339);
    let n912: ZB = zb_or(n884, n907);
    let n913: ZN = zsel_n(n148, n895, n908);
    let n914: ZN = zsel_n(n148, n896, n909);
    let n915: ZN = zsel_n(n148, n235, n910);
    let n916: ZN = zsel_n(n148, r_c339, n911);
    let n917: ZB = zb_or(n897, n912);
    let n918: ZN = zsel_n(n184, r_c241, n913);
    let n919: ZN = zsel_n(n184, r_c253, n914);
    let n920: ZN = zsel_n(n184, r_c257, n915);
    let n921: ZN = zsel_n(n184, r_c337, n890);
    let n922: ZN = zsel_n(n184, r_c339, n916);
    let n923: ZB = zb_or(n184, n917);
    let n924: ZB = zb_and(n855, n923);
    let n925: ZB = zb_and(n856, n923);
    let n926: ZB = zb_or(n924, n925);
    let n929: ZW = zw_bits_n(r_c39);
    let n930: ZW = zw_mix1(zw_splat(11400714819323198485u64), n929, 39u64);
    let n931: ZW = zw_mix2(zw_splat(11562461410679940143u64), n929, 39u64);
    let n932: ZW = zw_bits_n(n114);
    let n933: ZW = zw_mix1(n930, n932, 84u64);
    let n934: ZW = zw_mix2(n931, n932, 84u64);
    let n935: ZW = zw_bits_n(n170);
    let n936: ZW = zw_mix1(n933, n935, 85u64);
    let n937: ZW = zw_mix2(n934, n935, 85u64);
    let n938: ZW = zw_bits_n(n169);
    let n939: ZW = zw_mix1(n936, n938, 86u64);
    let n940: ZW = zw_mix2(n937, n938, 86u64);
    let n941: ZW = zw_bits_n(r_c87);
    let n942: ZW = zw_mix1(n939, n941, 87u64);
    let n943: ZW = zw_mix2(n940, n941, 87u64);
    let n944: ZW = zw_bits_n(n235);
    let n945: ZW = zw_mix1(n942, n944, 300u64);
    let n946: ZW = zw_mix2(n943, n944, 300u64);
    let n947: ZW = zw_bits_n(r_c20);
    let n948: ZW = zw_mix1(n945, n947, 20u64);
    let n949: ZW = zw_mix2(n946, n947, 20u64);
    let n950: u64 = false as u64;
    let n951: ZW = zw_mix1(n948, zw_splat(n950), 41u64);
    let n952: ZW = zw_mix2(n949, zw_splat(n950), 41u64);
    let n953: u64 = P8::from_raw(-65536i32).as_raw_u32() as u64;
    let n954: ZW = zw_mix1(n951, zw_splat(n953), 280u64);
    let n955: ZW = zw_mix2(n952, zw_splat(n953), 280u64);
    let n956: u64 = P8::from_raw(0i32).as_raw_u32() as u64;
    let n957: ZW = zw_mix1(n954, zw_splat(n956), 282u64);
    let n958: ZW = zw_mix2(n955, zw_splat(n956), 282u64);
    let n959: u64 = P8::from_raw(65536i32).as_raw_u32() as u64;
    let n960: ZW = zw_mix1(n957, zw_splat(n959), 283u64);
    let n961: ZW = zw_mix2(n958, zw_splat(n959), 283u64);
    let n962: ZW = zw_bits_n(n432);
    let n963: ZW = zw_mix1(n960, n962, 285u64);
    let n964: ZW = zw_mix2(n961, n962, 285u64);
    let n965: ZW = zw_mix1(n963, zw_splat(n950), 292u64);
    let n966: ZW = zw_mix2(n964, zw_splat(n950), 292u64);
    let n967: ZW = zw_mix1(n965, zw_splat(n950), 293u64);
    let n968: ZW = zw_mix2(n966, zw_splat(n950), 293u64);
    let n969: ZW = zw_mix1(n967, zw_splat(n956), 356u64);
    let n970: ZW = zw_mix2(n968, zw_splat(n956), 356u64);
    let n971: ZW = zw_mix1(n969, zw_splat(n956), 357u64);
    let n972: ZW = zw_mix2(n970, zw_splat(n956), 357u64);
    let n973: ZW = zw_mix1(n971, zw_splat(n956), 358u64);
    let n974: ZW = zw_mix2(n972, zw_splat(n956), 358u64);
    let n975: ZW = zw_mix1(n973, zw_splat(n956), 359u64);
    let n976: ZW = zw_mix2(n974, zw_splat(n956), 359u64);
    let n977: ZW = zw_mix1(n975, zw_splat(n950), 360u64);
    let n978: ZW = zw_mix2(n976, zw_splat(n950), 360u64);
    let n979: ZW = zw_mix1(n977, zw_splat(n956), 368u64);
    let n980: ZW = zw_mix2(n978, zw_splat(n956), 368u64);
    let n981: ZW = zw_bits_n(n443);
    let n982: ZW = zw_mix1(n979, n981, 369u64);
    let n983: ZW = zw_mix2(n980, n981, 369u64);
    let n984: u64 = true as u64;
    let n985: ZW = zw_mix1(n975, zw_splat(n984), 360u64);
    let n986: ZW = zw_mix2(n976, zw_splat(n984), 360u64);
    let n987: ZW = zw_bits_n(n472);
    let n988: ZW = zw_mix1(n985, n987, 368u64);
    let n989: ZW = zw_mix2(n986, n987, 368u64);
    let n990: ZW = zw_mix1(n988, n981, 369u64);
    let n991: ZW = zw_mix2(n989, n981, 369u64);
    let n992: ZW = zw_bits_n(n497);
    let n993: ZW = zw_mix1(n977, n992, 368u64);
    let n994: ZW = zw_mix2(n978, n992, 368u64);
    let n995: ZW = zw_mix1(n993, n981, 369u64);
    let n996: ZW = zw_mix2(n994, n981, 369u64);
    let n997: ZW = zw_bits_n(n459);
    let n998: ZW = zw_mix1(n960, n997, 285u64);
    let n999: ZW = zw_mix2(n961, n997, 285u64);
    let n1000: ZW = zw_mix1(n998, zw_splat(n950), 292u64);
    let n1001: ZW = zw_mix2(n999, zw_splat(n950), 292u64);
    let n1002: ZW = zw_mix1(n1000, zw_splat(n984), 293u64);
    let n1003: ZW = zw_mix2(n1001, zw_splat(n984), 293u64);
    let n1004: ZW = zw_mix1(n1002, zw_splat(n956), 356u64);
    let n1005: ZW = zw_mix2(n1003, zw_splat(n956), 356u64);
    let n1006: ZW = zw_mix1(n1004, zw_splat(n956), 357u64);
    let n1007: ZW = zw_mix2(n1005, zw_splat(n956), 357u64);
    let n1008: ZW = zw_mix1(n1006, zw_splat(n956), 358u64);
    let n1009: ZW = zw_mix2(n1007, zw_splat(n956), 358u64);
    let n1010: ZW = zw_mix1(n1008, zw_splat(n956), 359u64);
    let n1011: ZW = zw_mix2(n1009, zw_splat(n956), 359u64);
    let n1012: ZW = zw_mix1(n1010, zw_splat(n950), 360u64);
    let n1013: ZW = zw_mix2(n1011, zw_splat(n950), 360u64);
    let n1014: ZW = zw_bits_n(n460);
    let n1015: ZW = zw_mix1(n1012, n1014, 368u64);
    let n1016: ZW = zw_mix2(n1013, n1014, 368u64);
    let n1017: ZW = zw_bits_n(n461);
    let n1018: ZW = zw_mix1(n1015, n1017, 369u64);
    let n1019: ZW = zw_mix2(n1016, n1017, 369u64);
    let n1020: ZW = zw_mix1(n1010, zw_splat(n984), 360u64);
    let n1021: ZW = zw_mix2(n1011, zw_splat(n984), 360u64);
    let n1022: ZW = zw_bits_n(n491);
    let n1023: ZW = zw_mix1(n1020, n1022, 368u64);
    let n1024: ZW = zw_mix2(n1021, n1022, 368u64);
    let n1025: ZW = zw_mix1(n1023, n1017, 369u64);
    let n1026: ZW = zw_mix2(n1024, n1017, 369u64);
    let n1027: ZW = zw_bits_n(n516);
    let n1028: ZW = zw_mix1(n1012, n1027, 368u64);
    let n1029: ZW = zw_mix2(n1013, n1027, 368u64);
    let n1030: ZW = zw_mix1(n1028, n1017, 369u64);
    let n1031: ZW = zw_mix2(n1029, n1017, 369u64);
    let n1032: u64 = P8::from_raw(131072i32).as_raw_u32() as u64;
    let n1033: ZW = zw_mix1(n945, zw_splat(n1032), 20u64);
    let n1034: ZW = zw_mix2(n946, zw_splat(n1032), 20u64);
    let n1035: ZW = zw_mix1(n1033, zw_splat(n984), 41u64);
    let n1036: ZW = zw_mix2(n1034, zw_splat(n984), 41u64);
    let n1037: u64 = P8::from_raw(655360i32).as_raw_u32() as u64;
    let n1038: ZW = zw_mix1(n1035, zw_splat(n1037), 280u64);
    let n1039: ZW = zw_mix2(n1036, zw_splat(n1037), 280u64);
    let n1040: u64 = P8::from_raw(262144i32).as_raw_u32() as u64;
    let n1041: ZW = zw_mix1(n1038, zw_splat(n1040), 282u64);
    let n1042: ZW = zw_mix2(n1039, zw_splat(n1040), 282u64);
    let n1043: ZW = zw_mix1(n1041, zw_splat(n956), 283u64);
    let n1044: ZW = zw_mix2(n1042, zw_splat(n956), 283u64);
    let n1045: ZW = zw_mix1(n1043, n962, 285u64);
    let n1046: ZW = zw_mix2(n1044, n962, 285u64);
    let n1047: ZW = zw_mix1(n1045, zw_splat(n984), 292u64);
    let n1048: ZW = zw_mix2(n1046, zw_splat(n984), 292u64);
    let n1049: ZW = zw_mix1(n1047, zw_splat(n950), 293u64);
    let n1050: ZW = zw_mix2(n1048, zw_splat(n950), 293u64);
    let n1051: u64 = P8::from_raw(98304i32).as_raw_u32() as u64;
    let n1052: ZW = zw_mix1(n1049, zw_splat(n1051), 356u64);
    let n1053: ZW = zw_mix2(n1050, zw_splat(n1051), 356u64);
    let n1054: u64 = P8::from_raw(69510i32).as_raw_u32() as u64;
    let n1055: ZW = zw_mix1(n1052, zw_splat(n1054), 357u64);
    let n1056: ZW = zw_mix2(n1053, zw_splat(n1054), 357u64);
    let n1057: ZW = zw_mix1(n1055, zw_splat(n1032), 358u64);
    let n1058: ZW = zw_mix2(n1056, zw_splat(n1032), 358u64);
    let n1059: ZW = zw_mix1(n1057, zw_splat(n956), 359u64);
    let n1060: ZW = zw_mix2(n1058, zw_splat(n956), 359u64);
    let n1061: ZW = zw_mix1(n1059, zw_splat(n950), 360u64);
    let n1062: ZW = zw_mix2(n1060, zw_splat(n950), 360u64);
    let n1063: ZW = zw_mix1(n1061, zw_splat(n959), 368u64);
    let n1064: ZW = zw_mix2(n1062, zw_splat(n959), 368u64);
    let n1065: ZW = zw_mix1(n1063, zw_splat(n956), 369u64);
    let n1066: ZW = zw_mix2(n1064, zw_splat(n956), 369u64);
    let n1067: u64 = P8::from_raw(-131072i32).as_raw_u32() as u64;
    let n1068: ZW = zw_mix1(n1055, zw_splat(n1067), 358u64);
    let n1069: ZW = zw_mix2(n1056, zw_splat(n1067), 358u64);
    let n1070: ZW = zw_mix1(n1068, zw_splat(n956), 359u64);
    let n1071: ZW = zw_mix2(n1069, zw_splat(n956), 359u64);
    let n1072: ZW = zw_mix1(n1070, zw_splat(n984), 360u64);
    let n1073: ZW = zw_mix2(n1071, zw_splat(n984), 360u64);
    let n1074: u64 = P8::from_raw(-327680i32).as_raw_u32() as u64;
    let n1075: ZW = zw_mix1(n1072, zw_splat(n1074), 368u64);
    let n1076: ZW = zw_mix2(n1073, zw_splat(n1074), 368u64);
    let n1077: ZW = zw_mix1(n1075, zw_splat(n956), 369u64);
    let n1078: ZW = zw_mix2(n1076, zw_splat(n956), 369u64);
    let n1079: u64 = P8::from_raw(327680i32).as_raw_u32() as u64;
    let n1080: ZW = zw_mix1(n1061, zw_splat(n1079), 368u64);
    let n1081: ZW = zw_mix2(n1062, zw_splat(n1079), 368u64);
    let n1082: ZW = zw_mix1(n1080, zw_splat(n956), 369u64);
    let n1083: ZW = zw_mix2(n1081, zw_splat(n956), 369u64);
    let n1084: ZW = zw_mix1(n1049, zw_splat(n1054), 356u64);
    let n1085: ZW = zw_mix2(n1050, zw_splat(n1054), 356u64);
    let n1086: ZW = zw_mix1(n1084, zw_splat(n1051), 357u64);
    let n1087: ZW = zw_mix2(n1085, zw_splat(n1051), 357u64);
    let n1088: ZW = zw_mix1(n1086, zw_splat(n956), 358u64);
    let n1089: ZW = zw_mix2(n1087, zw_splat(n956), 358u64);
    let n1090: u64 = P8::from_raw(-98304i32).as_raw_u32() as u64;
    let n1091: ZW = zw_mix1(n1088, zw_splat(n1090), 359u64);
    let n1092: ZW = zw_mix2(n1089, zw_splat(n1090), 359u64);
    let n1093: ZW = zw_mix1(n1091, zw_splat(n950), 360u64);
    let n1094: ZW = zw_mix2(n1092, zw_splat(n950), 360u64);
    let n1095: ZW = zw_mix1(n1093, zw_splat(n956), 368u64);
    let n1096: ZW = zw_mix2(n1094, zw_splat(n956), 368u64);
    let n1097: ZW = zw_mix1(n1095, zw_splat(n1074), 369u64);
    let n1098: ZW = zw_mix2(n1096, zw_splat(n1074), 369u64);
    let n1099: ZW = zw_mix1(n1084, zw_splat(n1054), 357u64);
    let n1100: ZW = zw_mix2(n1085, zw_splat(n1054), 357u64);
    let n1101: ZW = zw_mix1(n1099, zw_splat(n1067), 358u64);
    let n1102: ZW = zw_mix2(n1100, zw_splat(n1067), 358u64);
    let n1103: ZW = zw_mix1(n1101, zw_splat(n1090), 359u64);
    let n1104: ZW = zw_mix2(n1102, zw_splat(n1090), 359u64);
    let n1105: ZW = zw_mix1(n1103, zw_splat(n984), 360u64);
    let n1106: ZW = zw_mix2(n1104, zw_splat(n984), 360u64);
    let n1107: u64 = P8::from_raw(-231700i32).as_raw_u32() as u64;
    let n1108: ZW = zw_mix1(n1105, zw_splat(n1107), 368u64);
    let n1109: ZW = zw_mix2(n1106, zw_splat(n1107), 368u64);
    let n1110: ZW = zw_mix1(n1108, zw_splat(n1107), 369u64);
    let n1111: ZW = zw_mix2(n1109, zw_splat(n1107), 369u64);
    let n1112: ZW = zw_mix1(n1099, zw_splat(n1032), 358u64);
    let n1113: ZW = zw_mix2(n1100, zw_splat(n1032), 358u64);
    let n1114: ZW = zw_mix1(n1112, zw_splat(n1090), 359u64);
    let n1115: ZW = zw_mix2(n1113, zw_splat(n1090), 359u64);
    let n1116: ZW = zw_mix1(n1114, zw_splat(n950), 360u64);
    let n1117: ZW = zw_mix2(n1115, zw_splat(n950), 360u64);
    let n1118: u64 = P8::from_raw(231700i32).as_raw_u32() as u64;
    let n1119: ZW = zw_mix1(n1116, zw_splat(n1118), 368u64);
    let n1120: ZW = zw_mix2(n1117, zw_splat(n1118), 368u64);
    let n1121: ZW = zw_mix1(n1119, zw_splat(n1107), 369u64);
    let n1122: ZW = zw_mix2(n1120, zw_splat(n1107), 369u64);
    let n1123: ZW = zw_mix1(n1088, zw_splat(n1032), 359u64);
    let n1124: ZW = zw_mix2(n1089, zw_splat(n1032), 359u64);
    let n1125: ZW = zw_mix1(n1123, zw_splat(n950), 360u64);
    let n1126: ZW = zw_mix2(n1124, zw_splat(n950), 360u64);
    let n1127: ZW = zw_mix1(n1125, zw_splat(n956), 368u64);
    let n1128: ZW = zw_mix2(n1126, zw_splat(n956), 368u64);
    let n1129: ZW = zw_mix1(n1127, zw_splat(n1079), 369u64);
    let n1130: ZW = zw_mix2(n1128, zw_splat(n1079), 369u64);
    let n1131: ZW = zw_mix1(n1101, zw_splat(n1032), 359u64);
    let n1132: ZW = zw_mix2(n1102, zw_splat(n1032), 359u64);
    let n1133: ZW = zw_mix1(n1131, zw_splat(n984), 360u64);
    let n1134: ZW = zw_mix2(n1132, zw_splat(n984), 360u64);
    let n1135: ZW = zw_mix1(n1133, zw_splat(n1107), 368u64);
    let n1136: ZW = zw_mix2(n1134, zw_splat(n1107), 368u64);
    let n1137: ZW = zw_mix1(n1135, zw_splat(n1118), 369u64);
    let n1138: ZW = zw_mix2(n1136, zw_splat(n1118), 369u64);
    let n1139: ZW = zw_mix1(n1112, zw_splat(n1032), 359u64);
    let n1140: ZW = zw_mix2(n1113, zw_splat(n1032), 359u64);
    let n1141: ZW = zw_mix1(n1139, zw_splat(n950), 360u64);
    let n1142: ZW = zw_mix2(n1140, zw_splat(n950), 360u64);
    let n1143: ZW = zw_mix1(n1141, zw_splat(n1118), 368u64);
    let n1144: ZW = zw_mix2(n1142, zw_splat(n1118), 368u64);
    let n1145: ZW = zw_mix1(n1143, zw_splat(n1118), 369u64);
    let n1146: ZW = zw_mix2(n1144, zw_splat(n1118), 369u64);
    let n1147: ZW = zw_mix1(n1043, n997, 285u64);
    let n1148: ZW = zw_mix2(n1044, n997, 285u64);
    let n1149: ZW = zw_mix1(n1147, zw_splat(n984), 292u64);
    let n1150: ZW = zw_mix2(n1148, zw_splat(n984), 292u64);
    let n1151: ZW = zw_mix1(n1149, zw_splat(n984), 293u64);
    let n1152: ZW = zw_mix2(n1150, zw_splat(n984), 293u64);
    let n1153: ZW = zw_mix1(n1151, zw_splat(n1051), 356u64);
    let n1154: ZW = zw_mix2(n1152, zw_splat(n1051), 356u64);
    let n1155: ZW = zw_mix1(n1153, zw_splat(n1054), 357u64);
    let n1156: ZW = zw_mix2(n1154, zw_splat(n1054), 357u64);
    let n1157: ZW = zw_mix1(n1155, zw_splat(n1032), 358u64);
    let n1158: ZW = zw_mix2(n1156, zw_splat(n1032), 358u64);
    let n1159: ZW = zw_mix1(n1157, zw_splat(n956), 359u64);
    let n1160: ZW = zw_mix2(n1158, zw_splat(n956), 359u64);
    let n1161: ZW = zw_mix1(n1159, zw_splat(n950), 360u64);
    let n1162: ZW = zw_mix2(n1160, zw_splat(n950), 360u64);
    let n1163: ZW = zw_mix1(n1161, zw_splat(n959), 368u64);
    let n1164: ZW = zw_mix2(n1162, zw_splat(n959), 368u64);
    let n1165: ZW = zw_mix1(n1163, zw_splat(n956), 369u64);
    let n1166: ZW = zw_mix2(n1164, zw_splat(n956), 369u64);
    let n1167: ZW = zw_mix1(n1155, zw_splat(n1067), 358u64);
    let n1168: ZW = zw_mix2(n1156, zw_splat(n1067), 358u64);
    let n1169: ZW = zw_mix1(n1167, zw_splat(n956), 359u64);
    let n1170: ZW = zw_mix2(n1168, zw_splat(n956), 359u64);
    let n1171: ZW = zw_mix1(n1169, zw_splat(n984), 360u64);
    let n1172: ZW = zw_mix2(n1170, zw_splat(n984), 360u64);
    let n1173: ZW = zw_mix1(n1171, zw_splat(n1074), 368u64);
    let n1174: ZW = zw_mix2(n1172, zw_splat(n1074), 368u64);
    let n1175: ZW = zw_mix1(n1173, zw_splat(n956), 369u64);
    let n1176: ZW = zw_mix2(n1174, zw_splat(n956), 369u64);
    let n1177: ZW = zw_mix1(n1161, zw_splat(n1079), 368u64);
    let n1178: ZW = zw_mix2(n1162, zw_splat(n1079), 368u64);
    let n1179: ZW = zw_mix1(n1177, zw_splat(n956), 369u64);
    let n1180: ZW = zw_mix2(n1178, zw_splat(n956), 369u64);
    let n1181: ZW = zw_mix1(n1151, zw_splat(n1054), 356u64);
    let n1182: ZW = zw_mix2(n1152, zw_splat(n1054), 356u64);
    let n1183: ZW = zw_mix1(n1181, zw_splat(n1051), 357u64);
    let n1184: ZW = zw_mix2(n1182, zw_splat(n1051), 357u64);
    let n1185: ZW = zw_mix1(n1183, zw_splat(n956), 358u64);
    let n1186: ZW = zw_mix2(n1184, zw_splat(n956), 358u64);
    let n1187: ZW = zw_mix1(n1185, zw_splat(n1090), 359u64);
    let n1188: ZW = zw_mix2(n1186, zw_splat(n1090), 359u64);
    let n1189: ZW = zw_mix1(n1187, zw_splat(n950), 360u64);
    let n1190: ZW = zw_mix2(n1188, zw_splat(n950), 360u64);
    let n1191: ZW = zw_mix1(n1189, zw_splat(n956), 368u64);
    let n1192: ZW = zw_mix2(n1190, zw_splat(n956), 368u64);
    let n1193: ZW = zw_mix1(n1191, zw_splat(n1074), 369u64);
    let n1194: ZW = zw_mix2(n1192, zw_splat(n1074), 369u64);
    let n1195: ZW = zw_mix1(n1181, zw_splat(n1054), 357u64);
    let n1196: ZW = zw_mix2(n1182, zw_splat(n1054), 357u64);
    let n1197: ZW = zw_mix1(n1195, zw_splat(n1067), 358u64);
    let n1198: ZW = zw_mix2(n1196, zw_splat(n1067), 358u64);
    let n1199: ZW = zw_mix1(n1197, zw_splat(n1090), 359u64);
    let n1200: ZW = zw_mix2(n1198, zw_splat(n1090), 359u64);
    let n1201: ZW = zw_mix1(n1199, zw_splat(n984), 360u64);
    let n1202: ZW = zw_mix2(n1200, zw_splat(n984), 360u64);
    let n1203: ZW = zw_mix1(n1201, zw_splat(n1107), 368u64);
    let n1204: ZW = zw_mix2(n1202, zw_splat(n1107), 368u64);
    let n1205: ZW = zw_mix1(n1203, zw_splat(n1107), 369u64);
    let n1206: ZW = zw_mix2(n1204, zw_splat(n1107), 369u64);
    let n1207: ZW = zw_mix1(n1195, zw_splat(n1032), 358u64);
    let n1208: ZW = zw_mix2(n1196, zw_splat(n1032), 358u64);
    let n1209: ZW = zw_mix1(n1207, zw_splat(n1090), 359u64);
    let n1210: ZW = zw_mix2(n1208, zw_splat(n1090), 359u64);
    let n1211: ZW = zw_mix1(n1209, zw_splat(n950), 360u64);
    let n1212: ZW = zw_mix2(n1210, zw_splat(n950), 360u64);
    let n1213: ZW = zw_mix1(n1211, zw_splat(n1118), 368u64);
    let n1214: ZW = zw_mix2(n1212, zw_splat(n1118), 368u64);
    let n1215: ZW = zw_mix1(n1213, zw_splat(n1107), 369u64);
    let n1216: ZW = zw_mix2(n1214, zw_splat(n1107), 369u64);
    let n1217: ZW = zw_mix1(n1185, zw_splat(n1032), 359u64);
    let n1218: ZW = zw_mix2(n1186, zw_splat(n1032), 359u64);
    let n1219: ZW = zw_mix1(n1217, zw_splat(n950), 360u64);
    let n1220: ZW = zw_mix2(n1218, zw_splat(n950), 360u64);
    let n1221: ZW = zw_mix1(n1219, zw_splat(n956), 368u64);
    let n1222: ZW = zw_mix2(n1220, zw_splat(n956), 368u64);
    let n1223: ZW = zw_mix1(n1221, zw_splat(n1079), 369u64);
    let n1224: ZW = zw_mix2(n1222, zw_splat(n1079), 369u64);
    let n1225: ZW = zw_mix1(n1197, zw_splat(n1032), 359u64);
    let n1226: ZW = zw_mix2(n1198, zw_splat(n1032), 359u64);
    let n1227: ZW = zw_mix1(n1225, zw_splat(n984), 360u64);
    let n1228: ZW = zw_mix2(n1226, zw_splat(n984), 360u64);
    let n1229: ZW = zw_mix1(n1227, zw_splat(n1107), 368u64);
    let n1230: ZW = zw_mix2(n1228, zw_splat(n1107), 368u64);
    let n1231: ZW = zw_mix1(n1229, zw_splat(n1118), 369u64);
    let n1232: ZW = zw_mix2(n1230, zw_splat(n1118), 369u64);
    let n1233: ZW = zw_mix1(n1207, zw_splat(n1032), 359u64);
    let n1234: ZW = zw_mix2(n1208, zw_splat(n1032), 359u64);
    let n1235: ZW = zw_mix1(n1233, zw_splat(n950), 360u64);
    let n1236: ZW = zw_mix2(n1234, zw_splat(n950), 360u64);
    let n1237: ZW = zw_mix1(n1235, zw_splat(n1118), 368u64);
    let n1238: ZW = zw_mix2(n1236, zw_splat(n1118), 368u64);
    let n1239: ZW = zw_mix1(n1237, zw_splat(n1118), 369u64);
    let n1240: ZW = zw_mix2(n1238, zw_splat(n1118), 369u64);
    let n1241: ZW = zw_mix1(zw_splat(11400714819323198485u64), n932, 84u64);
    let n1242: ZW = zw_mix2(zw_splat(11562461410679940143u64), n932, 84u64);
    let n1243: ZW = zw_mix1(n1241, n935, 85u64);
    let n1244: ZW = zw_mix2(n1242, n935, 85u64);
    let n1245: ZW = zw_mix1(n1243, n938, 86u64);
    let n1246: ZW = zw_mix2(n1244, n938, 86u64);
    let n1247: ZW = zw_bits_n(n641);
    let n1248: ZW = zw_mix1(n1245, n1247, 87u64);
    let n1249: ZW = zw_mix2(n1246, n1247, 87u64);
    let n1250: ZW = zw_mix1(n1248, n947, 20u64);
    let n1251: ZW = zw_mix2(n1249, n947, 20u64);
    let n1252: ZW = zw_mix1(n1250, zw_splat(n950), 41u64);
    let n1253: ZW = zw_mix2(n1251, zw_splat(n950), 41u64);
    let n1254: ZW = zw_mix1(n1248, zw_splat(n1032), 20u64);
    let n1255: ZW = zw_mix2(n1249, zw_splat(n1032), 20u64);
    let n1256: ZW = zw_mix1(n1254, zw_splat(n984), 41u64);
    let n1257: ZW = zw_mix2(n1255, zw_splat(n984), 41u64);
    let n1258: ZW = zw_mix1(n1245, n947, 20u64);
    let n1259: ZW = zw_mix2(n1246, n947, 20u64);
    let n1260: ZW = zw_bits_b(n802);
    let n1261: ZW = zw_mix1(n1258, n1260, 38u64);
    let n1262: ZW = zw_mix2(n1259, n1260, 38u64);
    let n1263: ZW = zw_bits_n(n801);
    let n1264: ZW = zw_mix1(n1261, n1263, 39u64);
    let n1265: ZW = zw_mix2(n1262, n1263, 39u64);
    let n1266: ZW = zw_bits_n(n800);
    let n1267: ZW = zw_mix1(n1264, n1266, 87u64);
    let n1268: ZW = zw_mix2(n1265, n1266, 87u64);
    let n1269: ZW = zw_bits_b(n811);
    let n1270: ZW = zw_mix1(n1258, n1269, 38u64);
    let n1271: ZW = zw_mix2(n1259, n1269, 38u64);
    let n1272: ZW = zw_bits_n(n810);
    let n1273: ZW = zw_mix1(n1270, n1272, 39u64);
    let n1274: ZW = zw_mix2(n1271, n1272, 39u64);
    let n1275: ZW = zw_bits_n(n809);
    let n1276: ZW = zw_mix1(n1273, n1275, 87u64);
    let n1277: ZW = zw_mix2(n1274, n1275, 87u64);
    let n1278: ZW = zw_bits_b(n820);
    let n1279: ZW = zw_mix1(n1258, n1278, 38u64);
    let n1280: ZW = zw_mix2(n1259, n1278, 38u64);
    let n1281: ZW = zw_bits_n(n819);
    let n1282: ZW = zw_mix1(n1279, n1281, 39u64);
    let n1283: ZW = zw_mix2(n1280, n1281, 39u64);
    let n1284: ZW = zw_bits_n(n818);
    let n1285: ZW = zw_mix1(n1282, n1284, 87u64);
    let n1286: ZW = zw_mix2(n1283, n1284, 87u64);
    let n1287: ZW = zw_bits_b(n829);
    let n1288: ZW = zw_mix1(n1258, n1287, 38u64);
    let n1289: ZW = zw_mix2(n1259, n1287, 38u64);
    let n1290: ZW = zw_bits_n(n828);
    let n1291: ZW = zw_mix1(n1288, n1290, 39u64);
    let n1292: ZW = zw_mix2(n1289, n1290, 39u64);
    let n1293: ZW = zw_bits_n(n827);
    let n1294: ZW = zw_mix1(n1291, n1293, 87u64);
    let n1295: ZW = zw_mix2(n1292, n1293, 87u64);
    let n1296: ZW = zw_bits_b(n838);
    let n1297: ZW = zw_mix1(n1258, n1296, 38u64);
    let n1298: ZW = zw_mix2(n1259, n1296, 38u64);
    let n1299: ZW = zw_bits_n(n837);
    let n1300: ZW = zw_mix1(n1297, n1299, 39u64);
    let n1301: ZW = zw_mix2(n1298, n1299, 39u64);
    let n1302: ZW = zw_bits_n(n836);
    let n1303: ZW = zw_mix1(n1300, n1302, 87u64);
    let n1304: ZW = zw_mix2(n1301, n1302, 87u64);
    let n1305: ZW = zw_bits_b(n847);
    let n1306: ZW = zw_mix1(n1258, n1305, 38u64);
    let n1307: ZW = zw_mix2(n1259, n1305, 38u64);
    let n1308: ZW = zw_bits_n(n846);
    let n1309: ZW = zw_mix1(n1306, n1308, 39u64);
    let n1310: ZW = zw_mix2(n1307, n1308, 39u64);
    let n1311: ZW = zw_bits_n(n845);
    let n1312: ZW = zw_mix1(n1309, n1311, 87u64);
    let n1313: ZW = zw_mix2(n1310, n1311, 87u64);
    let n1314: ZW = zw_mix1(n1245, zw_splat(n1032), 20u64);
    let n1315: ZW = zw_mix2(n1246, zw_splat(n1032), 20u64);
    let n1316: ZW = zw_mix1(n1314, n1260, 38u64);
    let n1317: ZW = zw_mix2(n1315, n1260, 38u64);
    let n1318: ZW = zw_mix1(n1316, n1263, 39u64);
    let n1319: ZW = zw_mix2(n1317, n1263, 39u64);
    let n1320: ZW = zw_mix1(n1318, n1266, 87u64);
    let n1321: ZW = zw_mix2(n1319, n1266, 87u64);
    let n1322: ZW = zw_mix1(n1314, n1269, 38u64);
    let n1323: ZW = zw_mix2(n1315, n1269, 38u64);
    let n1324: ZW = zw_mix1(n1322, n1272, 39u64);
    let n1325: ZW = zw_mix2(n1323, n1272, 39u64);
    let n1326: ZW = zw_mix1(n1324, n1275, 87u64);
    let n1327: ZW = zw_mix2(n1325, n1275, 87u64);
    let n1328: ZW = zw_mix1(n1314, n1278, 38u64);
    let n1329: ZW = zw_mix2(n1315, n1278, 38u64);
    let n1330: ZW = zw_mix1(n1328, n1281, 39u64);
    let n1331: ZW = zw_mix2(n1329, n1281, 39u64);
    let n1332: ZW = zw_mix1(n1330, n1284, 87u64);
    let n1333: ZW = zw_mix2(n1331, n1284, 87u64);
    let n1334: ZW = zw_mix1(n1314, n1287, 38u64);
    let n1335: ZW = zw_mix2(n1315, n1287, 38u64);
    let n1336: ZW = zw_mix1(n1334, n1290, 39u64);
    let n1337: ZW = zw_mix2(n1335, n1290, 39u64);
    let n1338: ZW = zw_mix1(n1336, n1293, 87u64);
    let n1339: ZW = zw_mix2(n1337, n1293, 87u64);
    let n1340: ZW = zw_mix1(n1314, n1296, 38u64);
    let n1341: ZW = zw_mix2(n1315, n1296, 38u64);
    let n1342: ZW = zw_mix1(n1340, n1299, 39u64);
    let n1343: ZW = zw_mix2(n1341, n1299, 39u64);
    let n1344: ZW = zw_mix1(n1342, n1302, 87u64);
    let n1345: ZW = zw_mix2(n1343, n1302, 87u64);
    let n1346: ZW = zw_mix1(n1314, n1305, 38u64);
    let n1347: ZW = zw_mix2(n1315, n1305, 38u64);
    let n1348: ZW = zw_mix1(n1346, n1308, 39u64);
    let n1349: ZW = zw_mix2(n1347, n1308, 39u64);
    let n1350: ZW = zw_mix1(n1348, n1311, 87u64);
    let n1351: ZW = zw_mix2(n1349, n1311, 87u64);
    let n1352: ZW = zw_bits_n(n854);
    let n1353: ZW = zw_mix1(zw_splat(11400714819323198485u64), n1352, 20u64);
    let n1354: ZW = zw_mix2(zw_splat(11562461410679940143u64), n1352, 20u64);
    let n1355: ZW = zw_mix1(n1353, n929, 39u64);
    let n1356: ZW = zw_mix2(n1354, n929, 39u64);
    let n1357: ZW = zw_mix1(n1355, n932, 84u64);
    let n1358: ZW = zw_mix2(n1356, n932, 84u64);
    let n1359: ZW = zw_mix1(n1357, n935, 85u64);
    let n1360: ZW = zw_mix2(n1358, n935, 85u64);
    let n1361: ZW = zw_mix1(n1359, n938, 86u64);
    let n1362: ZW = zw_mix2(n1360, n938, 86u64);
    let n1363: ZW = zw_mix1(n1361, n941, 87u64);
    let n1364: ZW = zw_mix2(n1362, n941, 87u64);
    let n1365: ZW = zw_bits_n(n918);
    let n1366: ZW = zw_mix1(n1363, n1365, 241u64);
    let n1367: ZW = zw_mix2(n1364, n1365, 241u64);
    let n1368: ZW = zw_bits_n(n887);
    let n1369: ZW = zw_mix1(n1366, n1368, 252u64);
    let n1370: ZW = zw_mix2(n1367, n1368, 252u64);
    let n1371: ZW = zw_bits_n(n919);
    let n1372: ZW = zw_mix1(n1369, n1371, 253u64);
    let n1373: ZW = zw_mix2(n1370, n1371, 253u64);
    let n1374: ZW = zw_bits_n(n920);
    let n1375: ZW = zw_mix1(n1372, n1374, 257u64);
    let n1376: ZW = zw_mix2(n1373, n1374, 257u64);
    let n1377: ZW = zw_bits_n(n921);
    let n1378: ZW = zw_mix1(n1375, n1377, 337u64);
    let n1379: ZW = zw_mix2(n1376, n1377, 337u64);
    let n1380: ZW = zw_bits_n(n922);
    let n1381: ZW = zw_mix1(n1378, n1380, 339u64);
    let n1382: ZW = zw_mix2(n1379, n1380, 339u64);
    let ok_v0_b0: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v0_b0: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v0_b0: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n466);
    let ok_v1_b1: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v1_b1: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v1_b1: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n494);
    let ok_v2_b2: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v2_b2: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v2_b2: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n519);
    let ok_v16_b3: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v16_b3: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v16_b3: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n548);
    let ok_v17_b4: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v17_b4: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v17_b4: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n577);
    let ok_v18_b5: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v18_b5: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v18_b5: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n606);
    let ok_v32_b6: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v32_b6: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v32_b6: u16 = ALL & zb_holds(n463) & zb_holds(n466);
    let ok_v33_b7: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v33_b7: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v33_b7: u16 = ALL & zb_holds(n463) & zb_holds(n494);
    let ok_v34_b8: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v34_b8: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v34_b8: u16 = ALL & zb_holds(n463) & zb_holds(n519);
    let ok_v36_b9: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v36_b9: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v36_b9: u16 = ALL & zb_holds(n463) & zb_holds(n466);
    let ok_v37_b10: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v37_b10: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v37_b10: u16 = ALL & zb_holds(n463) & zb_holds(n494);
    let ok_v38_b11: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v38_b11: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v38_b11: u16 = ALL & zb_holds(n463) & zb_holds(n519);
    let ok_v40_b12: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v40_b12: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v40_b12: u16 = ALL & zb_holds(n463) & zb_holds(n466);
    let ok_v41_b13: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v41_b13: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v41_b13: u16 = ALL & zb_holds(n463) & zb_holds(n494);
    let ok_v42_b14: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v42_b14: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v42_b14: u16 = ALL & zb_holds(n463) & zb_holds(n519);
    let ok_v48_b15: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v48_b15: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v48_b15: u16 = ALL & zb_holds(n463) & zb_holds(n548);
    let ok_v49_b16: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v49_b16: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v49_b16: u16 = ALL & zb_holds(n463) & zb_holds(n577);
    let ok_v50_b17: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v50_b17: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v50_b17: u16 = ALL & zb_holds(n463) & zb_holds(n606);
    let ok_v52_b18: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v52_b18: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v52_b18: u16 = ALL & zb_holds(n463) & zb_holds(n548);
    let ok_v53_b19: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v53_b19: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v53_b19: u16 = ALL & zb_holds(n463) & zb_holds(n577);
    let ok_v54_b20: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v54_b20: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v54_b20: u16 = ALL & zb_holds(n463) & zb_holds(n606);
    let ok_v56_b21: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v56_b21: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v56_b21: u16 = ALL & zb_holds(n463) & zb_holds(n548);
    let ok_v57_b22: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v57_b22: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v57_b22: u16 = ALL & zb_holds(n463) & zb_holds(n577);
    let ok_v58_b23: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n418);
    let bd_v58_b23: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v58_b23: u16 = ALL & zb_holds(n463) & zb_holds(n606);
    let ok_v0_b24: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v0_b24: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v0_b24: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n661);
    let ok_v1_b25: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v1_b25: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v1_b25: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n682);
    let ok_v2_b26: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v2_b26: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v2_b26: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n702);
    let ok_v16_b27: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v16_b27: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v16_b27: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n731);
    let ok_v17_b28: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v17_b28: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v17_b28: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n760);
    let ok_v18_b29: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v18_b29: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v18_b29: u16 = ALL & zb_holds(n130) & zb_holds(n463) & zb_holds(n789);
    let ok_v32_b30: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v32_b30: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v32_b30: u16 = ALL & zb_holds(n463) & zb_holds(n661);
    let ok_v33_b31: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v33_b31: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v33_b31: u16 = ALL & zb_holds(n463) & zb_holds(n682);
    let ok_v34_b32: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v34_b32: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v34_b32: u16 = ALL & zb_holds(n463) & zb_holds(n702);
    let ok_v48_b33: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v48_b33: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v48_b33: u16 = ALL & zb_holds(n463) & zb_holds(n731);
    let ok_v49_b34: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v49_b34: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v49_b34: u16 = ALL & zb_holds(n463) & zb_holds(n760);
    let ok_v50_b35: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n643);
    let bd_v50_b35: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v50_b35: u16 = ALL & zb_holds(n463) & zb_holds(n789);
    let ok_v0_b36: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n804);
    let bd_v0_b36: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v0_b36: u16 = ALL & zb_holds(n130) & zb_holds(n803);
    let ok_v1_b37: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n813);
    let bd_v1_b37: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v1_b37: u16 = ALL & zb_holds(n130) & zb_holds(n812);
    let ok_v2_b38: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n822);
    let bd_v2_b38: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v2_b38: u16 = ALL & zb_holds(n130) & zb_holds(n821);
    let ok_v16_b39: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n831);
    let bd_v16_b39: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v16_b39: u16 = ALL & zb_holds(n130) & zb_holds(n830);
    let ok_v17_b40: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n840);
    let bd_v17_b40: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v17_b40: u16 = ALL & zb_holds(n130) & zb_holds(n839);
    let ok_v18_b41: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n849);
    let bd_v18_b41: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v18_b41: u16 = ALL & zb_holds(n130) & zb_holds(n848);
    let ok_v32_b42: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n804);
    let bd_v32_b42: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v32_b42: u16 = ALL & zb_holds(n803);
    let ok_v33_b43: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n813);
    let bd_v33_b43: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v33_b43: u16 = ALL & zb_holds(n812);
    let ok_v34_b44: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n822);
    let bd_v34_b44: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v34_b44: u16 = ALL & zb_holds(n821);
    let ok_v48_b45: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n831);
    let bd_v48_b45: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v48_b45: u16 = ALL & zb_holds(n830);
    let ok_v49_b46: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n840);
    let bd_v49_b46: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v49_b46: u16 = ALL & zb_holds(n839);
    let ok_v50_b47: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95) & zb_holds(n849);
    let bd_v50_b47: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v50_b47: u16 = ALL & zb_holds(n848);
    let ok_v0_b48: u16 = ALL & zb_holds(n164) & zb_holds(n109) & zb_holds(n163) & zb_holds(n162) & zb_holds(n161) & zb_holds(n160) & zb_holds(n108) & zb_holds(r_c290) & zb_holds(n129) & zb_holds(n107) & zb_holds(n128) & zb_holds(n103) & zb_holds(n158) & zb_holds(n102) & zb_holds(r_c279) & zb_holds(n157) & zb_holds(n156) & zb_holds(n155) & zb_holds(n154) & zb_holds(n138) & zb_holds(r_c271) & zb_holds(n127) & zb_holds(n126) & zb_holds(n100) & zb_holds(n99) & zb_holds(n152) & zb_holds(n136) & zb_holds(r_c260) & zb_holds(n151) & zb_holds(n150) & zb_holds(n149) & zb_holds(n98) & zb_holds(n147) & zb_holds(n97) & zb_holds(n144) & zb_holds(n133) & zb_holds(r_c240) & zb_holds(n132) & zb_holds(n131) & zb_holds(r_c175) & zb_holds(n95);
    let bd_v0_b48: bool = !n106 || !n105 || !n159 || !n104 || !n101 || !n125 || !n153 || !n137 || !n96 || !n146 || !n134 || !n145;
    let live_v0_b48: u16 = ALL & zb_holds(n926);
    let sh0 = KShared0 {
        c87: r_c87,
        c39: r_c39,
        c84: n114,
        c86: n169,
        c300: n235,
        c85: n170,
    };
    let sh1 = KShared1 {
        c87: n641,
        c84: n114,
        c86: n169,
        c85: n170,
    };
    let sh2 = KShared2 {
        c84: n114,
        c86: n169,
        c85: n170,
    };
    let sh3 = KShared3 {
        c87: r_c87,
        c39: r_c39,
        c84: n114,
        c20: n854,
        c86: n169,
        c241: n918,
        c337: n921,
        c339: n922,
        c252: n887,
        c253: n919,
        c257: n920,
        c85: n170,
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
        c356: zn_splat(P8::from_raw(0i32)),
        c357: zn_splat(P8::from_raw(0i32)),
        c280: zn_splat(P8::from_raw(-65536i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(false),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(0i32)),
        c369: n443,
        h1: n982, h2: n983,
    };
    // body 0: buttons 0x00, forks 0x0
    sink.o0(0, take_0_0, &sh0, &o0);
    declined |= live_v1_b1 & (if bd_v1_b1 { ALL } else { !ok_v1_b1 });
    take_0_1 |= live_v1_b1 & ok_v1_b1 & (if bd_v1_b1 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c356: zn_splat(P8::from_raw(0i32)),
        c357: zn_splat(P8::from_raw(0i32)),
        c280: zn_splat(P8::from_raw(-65536i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: zb_splat(true),
        c285: n432,
        c292: zb_splat(false),
        c293: zb_splat(false),
        c368: n472,
        c369: n443,
        h1: n990, h2: n991,
    };
    // body 1: buttons 0x01, forks 0x0
    sink.o0(1, take_0_1, &sh0, &o0);
    declined |= live_v2_b2 & (if bd_v2_b2 { ALL } else { !ok_v2_b2 });
    take_0_2 |= live_v2_b2 & ok_v2_b2 & (if bd_v2_b2 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c356: zn_splat(P8::from_raw(0i32)),
        c357: zn_splat(P8::from_raw(0i32)),
        c280: zn_splat(P8::from_raw(-65536i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(false),
        c293: zb_splat(false),
        c368: n497,
        c369: n443,
        h1: n995, h2: n996,
    };
    // body 2: buttons 0x02, forks 0x0
    sink.o0(2, take_0_2, &sh0, &o0);
    declined |= live_v16_b3 & (if bd_v16_b3 { ALL } else { !ok_v16_b3 });
    take_0_3 |= live_v16_b3 & ok_v16_b3 & (if bd_v16_b3 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c356: zn_splat(P8::from_raw(0i32)),
        c357: zn_splat(P8::from_raw(0i32)),
        c280: zn_splat(P8::from_raw(-65536i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(false),
        c293: zb_splat(true),
        c368: n460,
        c369: n461,
        h1: n1018, h2: n1019,
    };
    // body 3: buttons 0x10, forks 0x0
    sink.o0(16, take_0_3, &sh0, &o0);
    declined |= live_v17_b4 & (if bd_v17_b4 { ALL } else { !ok_v17_b4 });
    take_0_4 |= live_v17_b4 & ok_v17_b4 & (if bd_v17_b4 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c356: zn_splat(P8::from_raw(0i32)),
        c357: zn_splat(P8::from_raw(0i32)),
        c280: zn_splat(P8::from_raw(-65536i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: zb_splat(true),
        c285: n459,
        c292: zb_splat(false),
        c293: zb_splat(true),
        c368: n491,
        c369: n461,
        h1: n1025, h2: n1026,
    };
    // body 4: buttons 0x11, forks 0x0
    sink.o0(17, take_0_4, &sh0, &o0);
    declined |= live_v18_b5 & (if bd_v18_b5 { ALL } else { !ok_v18_b5 });
    take_0_5 |= live_v18_b5 & ok_v18_b5 & (if bd_v18_b5 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: r_c20,
        c41: zb_splat(false),
        c356: zn_splat(P8::from_raw(0i32)),
        c357: zn_splat(P8::from_raw(0i32)),
        c280: zn_splat(P8::from_raw(-65536i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(0i32)),
        c283: zn_splat(P8::from_raw(65536i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(false),
        c293: zb_splat(true),
        c368: n516,
        c369: n461,
        h1: n1030, h2: n1031,
    };
    // body 5: buttons 0x12, forks 0x0
    sink.o0(18, take_0_5, &sh0, &o0);
    declined |= live_v32_b6 & (if bd_v32_b6 { ALL } else { !ok_v32_b6 });
    take_0_6 |= live_v32_b6 & ok_v32_b6 & (if bd_v32_b6 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(98304i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(65536i32)),
        c369: zn_splat(P8::from_raw(0i32)),
        h1: n1065, h2: n1066,
    };
    // body 6: buttons 0x20, forks 0x0
    sink.o0(32, take_0_6, &sh0, &o0);
    declined |= live_v33_b7 & (if bd_v33_b7 { ALL } else { !ok_v33_b7 });
    take_0_7 |= live_v33_b7 & ok_v33_b7 & (if bd_v33_b7 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(98304i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(-131072i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(true),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(-327680i32)),
        c369: zn_splat(P8::from_raw(0i32)),
        h1: n1077, h2: n1078,
    };
    // body 7: buttons 0x21, forks 0x0
    sink.o0(33, take_0_7, &sh0, &o0);
    declined |= live_v34_b8 & (if bd_v34_b8 { ALL } else { !ok_v34_b8 });
    take_0_8 |= live_v34_b8 & ok_v34_b8 & (if bd_v34_b8 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(98304i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(327680i32)),
        c369: zn_splat(P8::from_raw(0i32)),
        h1: n1082, h2: n1083,
    };
    // body 8: buttons 0x22, forks 0x0
    sink.o0(34, take_0_8, &sh0, &o0);
    declined |= live_v36_b9 & (if bd_v36_b9 { ALL } else { !ok_v36_b9 });
    take_0_9 |= live_v36_b9 & ok_v36_b9 & (if bd_v36_b9 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(98304i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(-98304i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(0i32)),
        c369: zn_splat(P8::from_raw(-327680i32)),
        h1: n1097, h2: n1098,
    };
    // body 9: buttons 0x24, forks 0x0
    sink.o0(36, take_0_9, &sh0, &o0);
    declined |= live_v37_b10 & (if bd_v37_b10 { ALL } else { !ok_v37_b10 });
    take_0_10 |= live_v37_b10 & ok_v37_b10 & (if bd_v37_b10 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(-131072i32)),
        c359: zn_splat(P8::from_raw(-98304i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(true),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(-231700i32)),
        c369: zn_splat(P8::from_raw(-231700i32)),
        h1: n1110, h2: n1111,
    };
    // body 10: buttons 0x25, forks 0x0
    sink.o0(37, take_0_10, &sh0, &o0);
    declined |= live_v38_b11 & (if bd_v38_b11 { ALL } else { !ok_v38_b11 });
    take_0_11 |= live_v38_b11 & ok_v38_b11 & (if bd_v38_b11 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(-98304i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(231700i32)),
        c369: zn_splat(P8::from_raw(-231700i32)),
        h1: n1121, h2: n1122,
    };
    // body 11: buttons 0x26, forks 0x0
    sink.o0(38, take_0_11, &sh0, &o0);
    declined |= live_v40_b12 & (if bd_v40_b12 { ALL } else { !ok_v40_b12 });
    take_0_12 |= live_v40_b12 & ok_v40_b12 & (if bd_v40_b12 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(98304i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(131072i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(0i32)),
        c369: zn_splat(P8::from_raw(327680i32)),
        h1: n1129, h2: n1130,
    };
    // body 12: buttons 0x28, forks 0x0
    sink.o0(40, take_0_12, &sh0, &o0);
    declined |= live_v41_b13 & (if bd_v41_b13 { ALL } else { !ok_v41_b13 });
    take_0_13 |= live_v41_b13 & ok_v41_b13 & (if bd_v41_b13 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(-131072i32)),
        c359: zn_splat(P8::from_raw(131072i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(true),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(-231700i32)),
        c369: zn_splat(P8::from_raw(231700i32)),
        h1: n1137, h2: n1138,
    };
    // body 13: buttons 0x29, forks 0x0
    sink.o0(41, take_0_13, &sh0, &o0);
    declined |= live_v42_b14 & (if bd_v42_b14 { ALL } else { !ok_v42_b14 });
    take_0_14 |= live_v42_b14 & ok_v42_b14 & (if bd_v42_b14 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(131072i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n432,
        c292: zb_splat(true),
        c293: zb_splat(false),
        c368: zn_splat(P8::from_raw(231700i32)),
        c369: zn_splat(P8::from_raw(231700i32)),
        h1: n1145, h2: n1146,
    };
    // body 14: buttons 0x2a, forks 0x0
    sink.o0(42, take_0_14, &sh0, &o0);
    declined |= live_v48_b15 & (if bd_v48_b15 { ALL } else { !ok_v48_b15 });
    take_0_15 |= live_v48_b15 & ok_v48_b15 & (if bd_v48_b15 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(98304i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(65536i32)),
        c369: zn_splat(P8::from_raw(0i32)),
        h1: n1165, h2: n1166,
    };
    // body 15: buttons 0x30, forks 0x0
    sink.o0(48, take_0_15, &sh0, &o0);
    declined |= live_v49_b16 & (if bd_v49_b16 { ALL } else { !ok_v49_b16 });
    take_0_16 |= live_v49_b16 & ok_v49_b16 & (if bd_v49_b16 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(98304i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(-131072i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(true),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(-327680i32)),
        c369: zn_splat(P8::from_raw(0i32)),
        h1: n1175, h2: n1176,
    };
    // body 16: buttons 0x31, forks 0x0
    sink.o0(49, take_0_16, &sh0, &o0);
    declined |= live_v50_b17 & (if bd_v50_b17 { ALL } else { !ok_v50_b17 });
    take_0_17 |= live_v50_b17 & ok_v50_b17 & (if bd_v50_b17 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(98304i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(0i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(327680i32)),
        c369: zn_splat(P8::from_raw(0i32)),
        h1: n1179, h2: n1180,
    };
    // body 17: buttons 0x32, forks 0x0
    sink.o0(50, take_0_17, &sh0, &o0);
    declined |= live_v52_b18 & (if bd_v52_b18 { ALL } else { !ok_v52_b18 });
    take_0_18 |= live_v52_b18 & ok_v52_b18 & (if bd_v52_b18 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(98304i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(-98304i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(0i32)),
        c369: zn_splat(P8::from_raw(-327680i32)),
        h1: n1193, h2: n1194,
    };
    // body 18: buttons 0x34, forks 0x0
    sink.o0(52, take_0_18, &sh0, &o0);
    declined |= live_v53_b19 & (if bd_v53_b19 { ALL } else { !ok_v53_b19 });
    take_0_19 |= live_v53_b19 & ok_v53_b19 & (if bd_v53_b19 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(-131072i32)),
        c359: zn_splat(P8::from_raw(-98304i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(true),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(-231700i32)),
        c369: zn_splat(P8::from_raw(-231700i32)),
        h1: n1205, h2: n1206,
    };
    // body 19: buttons 0x35, forks 0x0
    sink.o0(53, take_0_19, &sh0, &o0);
    declined |= live_v54_b20 & (if bd_v54_b20 { ALL } else { !ok_v54_b20 });
    take_0_20 |= live_v54_b20 & ok_v54_b20 & (if bd_v54_b20 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(-98304i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(231700i32)),
        c369: zn_splat(P8::from_raw(-231700i32)),
        h1: n1215, h2: n1216,
    };
    // body 20: buttons 0x36, forks 0x0
    sink.o0(54, take_0_20, &sh0, &o0);
    declined |= live_v56_b21 & (if bd_v56_b21 { ALL } else { !ok_v56_b21 });
    take_0_21 |= live_v56_b21 & ok_v56_b21 & (if bd_v56_b21 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(98304i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(0i32)),
        c359: zn_splat(P8::from_raw(131072i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(0i32)),
        c369: zn_splat(P8::from_raw(327680i32)),
        h1: n1223, h2: n1224,
    };
    // body 21: buttons 0x38, forks 0x0
    sink.o0(56, take_0_21, &sh0, &o0);
    declined |= live_v57_b22 & (if bd_v57_b22 { ALL } else { !ok_v57_b22 });
    take_0_22 |= live_v57_b22 & ok_v57_b22 & (if bd_v57_b22 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(-131072i32)),
        c359: zn_splat(P8::from_raw(131072i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(true),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(-231700i32)),
        c369: zn_splat(P8::from_raw(231700i32)),
        h1: n1231, h2: n1232,
    };
    // body 22: buttons 0x39, forks 0x0
    sink.o0(57, take_0_22, &sh0, &o0);
    declined |= live_v58_b23 & (if bd_v58_b23 { ALL } else { !ok_v58_b23 });
    take_0_23 |= live_v58_b23 & ok_v58_b23 & (if bd_v58_b23 { 0 } else { ALL });
    let o0 = KOut0 {
        c20: zn_splat(P8::from_raw(131072i32)),
        c41: zb_splat(true),
        c356: zn_splat(P8::from_raw(69510i32)),
        c357: zn_splat(P8::from_raw(69510i32)),
        c280: zn_splat(P8::from_raw(655360i32)),
        c358: zn_splat(P8::from_raw(131072i32)),
        c359: zn_splat(P8::from_raw(131072i32)),
        c282: zn_splat(P8::from_raw(262144i32)),
        c283: zn_splat(P8::from_raw(0i32)),
        c360: zb_splat(false),
        c285: n459,
        c292: zb_splat(true),
        c293: zb_splat(true),
        c368: zn_splat(P8::from_raw(231700i32)),
        c369: zn_splat(P8::from_raw(231700i32)),
        h1: n1239, h2: n1240,
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
        h1: n1252, h2: n1253,
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
        h1: n1256, h2: n1257,
    };
    // body 35: buttons 0x32, forks 0x0
    sink.o1(50, take_1_1, &sh1, &o1);
    declined |= live_v0_b36 & (if bd_v0_b36 { ALL } else { !ok_v0_b36 });
    take_2_0 |= live_v0_b36 & ok_v0_b36 & (if bd_v0_b36 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n800,
        c39: n801,
        c20: r_c20,
        c38: n802,
        h1: n1267, h2: n1268,
    };
    // body 36: buttons 0x00, forks 0x0
    sink.o2(0, take_2_0, &sh2, &o2);
    declined |= live_v1_b37 & (if bd_v1_b37 { ALL } else { !ok_v1_b37 });
    take_2_1 |= live_v1_b37 & ok_v1_b37 & (if bd_v1_b37 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n809,
        c39: n810,
        c20: r_c20,
        c38: n811,
        h1: n1276, h2: n1277,
    };
    // body 37: buttons 0x01, forks 0x0
    sink.o2(1, take_2_1, &sh2, &o2);
    declined |= live_v2_b38 & (if bd_v2_b38 { ALL } else { !ok_v2_b38 });
    take_2_2 |= live_v2_b38 & ok_v2_b38 & (if bd_v2_b38 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n818,
        c39: n819,
        c20: r_c20,
        c38: n820,
        h1: n1285, h2: n1286,
    };
    // body 38: buttons 0x02, forks 0x0
    sink.o2(2, take_2_2, &sh2, &o2);
    declined |= live_v16_b39 & (if bd_v16_b39 { ALL } else { !ok_v16_b39 });
    take_2_3 |= live_v16_b39 & ok_v16_b39 & (if bd_v16_b39 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n827,
        c39: n828,
        c20: r_c20,
        c38: n829,
        h1: n1294, h2: n1295,
    };
    // body 39: buttons 0x10, forks 0x0
    sink.o2(16, take_2_3, &sh2, &o2);
    declined |= live_v17_b40 & (if bd_v17_b40 { ALL } else { !ok_v17_b40 });
    take_2_4 |= live_v17_b40 & ok_v17_b40 & (if bd_v17_b40 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n836,
        c39: n837,
        c20: r_c20,
        c38: n838,
        h1: n1303, h2: n1304,
    };
    // body 40: buttons 0x11, forks 0x0
    sink.o2(17, take_2_4, &sh2, &o2);
    declined |= live_v18_b41 & (if bd_v18_b41 { ALL } else { !ok_v18_b41 });
    take_2_5 |= live_v18_b41 & ok_v18_b41 & (if bd_v18_b41 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n845,
        c39: n846,
        c20: r_c20,
        c38: n847,
        h1: n1312, h2: n1313,
    };
    // body 41: buttons 0x12, forks 0x0
    sink.o2(18, take_2_5, &sh2, &o2);
    declined |= live_v32_b42 & (if bd_v32_b42 { ALL } else { !ok_v32_b42 });
    take_2_6 |= live_v32_b42 & ok_v32_b42 & (if bd_v32_b42 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n800,
        c39: n801,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n802,
        h1: n1320, h2: n1321,
    };
    // body 42: buttons 0x20, forks 0x0
    sink.o2(32, take_2_6, &sh2, &o2);
    declined |= live_v33_b43 & (if bd_v33_b43 { ALL } else { !ok_v33_b43 });
    take_2_7 |= live_v33_b43 & ok_v33_b43 & (if bd_v33_b43 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n809,
        c39: n810,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n811,
        h1: n1326, h2: n1327,
    };
    // body 43: buttons 0x21, forks 0x0
    sink.o2(33, take_2_7, &sh2, &o2);
    declined |= live_v34_b44 & (if bd_v34_b44 { ALL } else { !ok_v34_b44 });
    take_2_8 |= live_v34_b44 & ok_v34_b44 & (if bd_v34_b44 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n818,
        c39: n819,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n820,
        h1: n1332, h2: n1333,
    };
    // body 44: buttons 0x22, forks 0x0
    sink.o2(34, take_2_8, &sh2, &o2);
    declined |= live_v48_b45 & (if bd_v48_b45 { ALL } else { !ok_v48_b45 });
    take_2_9 |= live_v48_b45 & ok_v48_b45 & (if bd_v48_b45 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n827,
        c39: n828,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n829,
        h1: n1338, h2: n1339,
    };
    // body 45: buttons 0x30, forks 0x0
    sink.o2(48, take_2_9, &sh2, &o2);
    declined |= live_v49_b46 & (if bd_v49_b46 { ALL } else { !ok_v49_b46 });
    take_2_10 |= live_v49_b46 & ok_v49_b46 & (if bd_v49_b46 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n836,
        c39: n837,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n838,
        h1: n1344, h2: n1345,
    };
    // body 46: buttons 0x31, forks 0x0
    sink.o2(49, take_2_10, &sh2, &o2);
    declined |= live_v50_b47 & (if bd_v50_b47 { ALL } else { !ok_v50_b47 });
    take_2_11 |= live_v50_b47 & ok_v50_b47 & (if bd_v50_b47 { 0 } else { ALL });
    let o2 = KOut2 {
        c87: n845,
        c39: n846,
        c20: zn_splat(P8::from_raw(131072i32)),
        c38: n847,
        h1: n1350, h2: n1351,
    };
    // body 47: buttons 0x32, forks 0x0
    sink.o2(50, take_2_11, &sh2, &o2);
    declined |= live_v0_b48 & (if bd_v0_b48 { ALL } else { !ok_v0_b48 });
    take_3_0 |= live_v0_b48 & ok_v0_b48 & (if bd_v0_b48 { 0 } else { ALL });
    let o3 = KOut3 {
        h1: n1381, h2: n1382,
    };
    // body 48: buttons 0x00, forks 0x0
    sink.o3(0, take_3_0, &sh3, &o3);
    declined
}
