//! Reading a block's rows by NAME, for the diagnostics (`rewrite ref-check`,
//! `follow`, `spurious`, `witness`, ...) that compare rows across shapes and
//! levels: two shapes number their cells differently, so a row is compared
//! as its named value cells.

use celeste_engine::runtime2::{BoundaryIds, Cell2, Col, Rt2, AV};
use std::collections::{BTreeMap, HashMap};

/// A row's named value cells, each shown (`av_show`).
pub type Proj = BTreeMap<String, String>;

/// A value cell for a diagnostic: numbers as decimals, intervals as ranges.
pub fn av_show(v: AV) -> String {
    match v {
        AV::Num(n) => format!("{}", n.as_raw_u32() as i32 as f64 / 65536.0),
        AV::Ival(a, b) => format!("[{}, {}]", a.as_raw_u32() as i32 as f64 / 65536.0, b.as_raw_u32() as i32 as f64 / 65536.0),
        other => format!("{other:?}"),
    }
}

/// A block's value cells by NAME: the player's position, speed and
/// remainder (`player.x`, `spd.x`, `rem.y`, `dash_effect_time`), and every
/// object's fields as `type[i].field` (and `type[i].field.sub` one table
/// deeper). A cell no name reaches (a global's) is left out.
pub fn cell_names(rt2: &Rt2, ids: &BoundaryIds) -> HashMap<usize, String> {
    let mut names = HashMap::new();
    if let Some(obj) = crate::search::pos_graph::player_object(rt2) {
        for (nm, f) in [("player.x", ids.f_x), ("player.y", ids.f_y), ("dash_effect_time", ids.f_dash_effect_time)] {
            if let Some(c) = rt2.obj_field_cell(obj, f) {
                names.insert(c as usize, nm.to_string());
            }
        }
        for (nm, f) in [("spd", ids.f_spd), ("rem", ids.f_rem)] {
            if let Some(pc) = rt2.obj_field_cell(obj, f) {
                if let Col::U(AV::Ptr(sub)) = rt2.cols[pc as usize] {
                    for (ax, g) in [("x", ids.f_x), ("y", ids.f_y)] {
                        if let Some(c) = rt2.obj_field_cell(sub, g) {
                            names.insert(c as usize, format!("{nm}.{ax}"));
                        }
                    }
                }
            }
        }
    }
    // Every other object's fields: the column that multiplies a frontier is
    // as often a platform's or a fall floor's as the player's.
    if let Some(arr) = rt2.global_target(ids.g_objects) {
        if let Cell2::Arr(items) = &rt2.structure[arr as usize] {
            let type_name = |t: u32| -> String {
                (0..celeste_names::GLOBAL_NAMES.len() as u32)
                    .find(|&g| rt2.global_target(g) == Some(t))
                    .map(|g| celeste_names::GLOBAL_NAMES[g as usize].to_string())
                    .unwrap_or_else(|| "?".to_string())
            };
            let field_name = |f: u32| celeste_names::FIELD_NAMES.get(f as usize).copied().unwrap_or("?");
            for (i, item) in items.iter().enumerate() {
                let Col::U(AV::Ptr(obj)) = rt2.cols[*item as usize] else { continue };
                let Cell2::Obj(fields) = &rt2.structure[obj as usize] else { continue };
                let ty = rt2
                    .obj_field_cell(obj, ids.f_type)
                    .and_then(|c| match rt2.cols[c as usize] {
                        Col::U(AV::Ptr(t)) => Some(type_name(t)),
                        _ => None,
                    })
                    .unwrap_or_else(|| "?".to_string());
                for &(f, c) in fields {
                    match (&rt2.structure[c as usize], &rt2.cols[c as usize]) {
                        (Cell2::Val, Col::U(AV::Ptr(sub))) => {
                            if let Cell2::Obj(subs) = &rt2.structure[*sub as usize] {
                                for &(g, c2) in subs {
                                    names.entry(c2 as usize).or_insert_with(|| format!("{ty}[{i}].{}.{}", field_name(f), field_name(g)));
                                }
                            }
                        }
                        (Cell2::Val, _) => {
                            names.entry(c as usize).or_insert_with(|| format!("{ty}[{i}].{}", field_name(f)));
                        }
                        _ => {}
                    }
                }
            }
        }
    }
    names
}

/// Row `r`'s projection: every named value cell (`cell_names`) but those
/// whose name starts with an `erase` prefix, and no pointers (structure,
/// renumbered between shapes).
pub fn project_row(rt2: &Rt2, names: &HashMap<usize, String>, r: u32, erase: &[String]) -> Proj {
    names
        .iter()
        .filter(|(c, n)| matches!(rt2.structure[**c], Cell2::Val) && !erase.iter().any(|e| n.starts_with(e.as_str())))
        .map(|(c, n)| (n.clone(), av_show(rt2.cols[*c].at(r as usize))))
        .filter(|(_, v)| !v.starts_with("Ptr("))
        .collect()
}

/// Every row of `rt2` projected (`project_row`, nothing erased).
pub fn project_all(rt2: &Rt2) -> Vec<Proj> {
    let names = cell_names(rt2, crate::compiled::ids());
    (0..rt2.width).map(|r| project_row(rt2, &names, r as u32, &[])).collect()
}

/// Every row of `rt2` widened onto `level` (`frame::widen_rt2_to`), projected.
pub fn project_onto(rt2: &mut Rt2, level: crate::interpreter::abstraction::Level) -> Vec<Proj> {
    crate::frame::widen_rt2_to(rt2, level);
    project_all(rt2)
}

/// A projection's hash, for set membership.
pub fn projection_key(p: &Proj) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = rustc_hash::FxHasher::default();
    p.hash(&mut h);
    h.finish()
}

/// The player's fields of a projection, for reading: position, remainder,
/// speed, dash, the player object's scalars.
pub fn brief(p: &Proj) -> String {
    p.iter()
        .filter(|(n, _)| {
            (n.starts_with("player") || n.starts_with("spd") || n.starts_with("rem") || n.starts_with("dash"))
                && !n.contains("hitbox")
                && !n.contains(".type")
                && !n.ends_with(".solids")
                && !n.ends_with(".collideable")
                && !n.contains(".flip.y")
        })
        .map(|(n, v)| format!("{n}={v}"))
        .collect::<Vec<_>>()
        .join(" ")
}

/// `brief` of lane `l`.
pub fn player_summary(rt2: &Rt2, l: usize) -> String {
    let names = cell_names(rt2, crate::compiled::ids());
    brief(&project_row(rt2, &names, l as u32, &[]))
}

/// The named fields where lane `la` of `a` and lane `lb` of `b` differ
/// (`project_row` of each, by name, so two structures compare): "name: X vs
/// Y", at most `cap` of them. A name only one side has is listed as missing.
pub fn field_diff(a: &Rt2, la: usize, b: &Rt2, lb: usize, cap: usize) -> Vec<String> {
    let ids = crate::compiled::ids();
    let pa = project_row(a, &cell_names(a, ids), la as u32, &[]);
    let pb = project_row(b, &cell_names(b, ids), lb as u32, &[]);
    let mut out = Vec::new();
    for (n, va) in &pa {
        match pb.get(n) {
            Some(vb) if vb == va => {}
            Some(vb) => out.push(format!("{n}: {va} vs {vb}")),
            None => out.push(format!("{n}: {va} vs (missing)")),
        }
    }
    for (n, vb) in &pb {
        if !pa.contains_key(n) {
            out.push(format!("{n}: (missing) vs {vb}"));
        }
    }
    out.truncate(cap);
    out
}

/// `"x,y"` as a pair (a clap `value_parser`).
pub fn parse_xy(s: &str) -> Result<(i32, i32), String> {
    let (a, b) = s.split_once(',').ok_or_else(|| format!("{s:?}: expected x,y"))?;
    let n = |t: &str| t.trim().parse::<i32>().map_err(|e| format!("{s:?}: {e}"));
    Ok((n(a)?, n(b)?))
}
