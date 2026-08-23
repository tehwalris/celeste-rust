//! Render a whole compilable kernel from a TRACED frame.
//!
//! `transpile::kernel::render` does this for the WALK - the recipe
//! pipeline's emitter - and this is deliberately not that function
//! generalized. The walk's renderer carries machinery a traced frame
//! never produces: `Op::Split` fork configurations, `SplitValid`,
//! `fork_depth`, `valid_expr`, and a shape witness to bind against.
//! Those exist because `zi_fork_flr` splits one interval lane into two.
//! A tracer's outcomes come from real branches instead, so generalizing
//! the walk's renderer would mean inheriting assumptions and then
//! noticing them one at a time.
//!
//! Two differences from the walk's generated interface are the point
//! rather than an accident.
//!
//! INPUTS ARE NAMED BY PATH. The walk binds by canonical cell id
//! against a shape witness; this emits the path each input came from, so
//! the binder resolves against whatever block it is handed
//! (`trace::bind`). The cell ids in the struct field names are just
//! names - they agree with the traced input block's numbering, which
//! makes them readable, and nothing depends on that.
//!
//! AN OUTPUT BLOCK IS BUILT, NOT PATCHED. The walk's `acc_init` clones
//! the chunk's structure and overwrites the columns the frame wrote,
//! because in that pipeline the output shape IS the input shape. A
//! traced frame ends in several shapes - room (0,0) f40 has four, and
//! only one of them is the input's - so each outcome carries its own
//! structure and the kernel fills it. Measured there: about 105 pointer
//! cells that the structure carries, ~53 scalars the frame computes, and
//! nothing that passes through from the input block.

use std::collections::BTreeMap;
use std::fmt::Write;

use anyhow::Result;

use celeste_engine::runtime2::{Cell2, Col, AV};

use super::emit::{Bound, Lowered};
use super::verify::Frame;
use crate::transpile::kernel::{render_lines, OutField};

/// The Rust type of an input of this kind, at block-uniform width.
fn uni_ty(kind: &str) -> &'static str {
    match kind {
        "num" => "P8",
        "bool" => "bool",
        _ => "IV",
    }
}

/// The Rust type of an input of this kind, per lane.
fn row_ty(kind: &str) -> &'static str {
    match kind {
        "num" => "ZN",
        _ => "u16",
    }
}

/// Render the kernel for one traced frame.
pub fn render(f: &Frame, b: &Bound, l: &Lowered, title: &str) -> Result<String> {
    let mut o = String::new();

    // cell -> the path it was read from, for the input side.
    let mut in_path: BTreeMap<u32, String> = BTreeMap::new();
    for (i, cell) in f.in_cells.iter().enumerate() {
        in_path.insert(*cell, super::iface::show(&f.iface.slots[i]));
    }

    writeln!(
        o,
        "// GENERATED from a TRACED frame ({}). Do not edit.\n\
         //\n\
         // One input shape, {} output shapes, {} distinct button\n\
         // assignments. See `trace::kernel` for what this interface is\n\
         // and why it is not the walk's.\n\
         #![allow(unused_variables, unused_mut, unused_imports, clippy::all)]\n\
         use celeste_engine::kernel::*;\n\
         use celeste_core::pico8_num::{{Pico8Num as P8, Pico8NumInterval as IV}};\n\
         use celeste_core::cart_data::CartData;\n\
         use celeste_core::collision_cache::CollisionCache;\n\
         use celeste_engine::runtime2::{{Rt2, Col, AV}};\n\
         use celeste_engine::slots::{{build_block, resolve_path, SCell}};\n\
         use std::sync::Arc;\n\
         \n\
         pub struct G<'a> {{ pub cart: &'a CartData, pub cache: &'a CollisionCache }}\n",
        title,
        b.outcomes.len(),
        l.variants.len()
    )?;

    // ---- inputs ----
    let slot_list = |o: &mut String, name: &str, cells: &[(u32, &'static str)]| -> Result<()> {
        writeln!(o, "/// (path, kind) - resolved against a block at bind time.")?;
        writeln!(o, "pub const {}: &[(&str, &str)] = &[", name)?;
        for (cell, kind) in cells {
            writeln!(o, "    ({:?}, {:?}),", in_path[cell].as_str(), kind)?;
        }
        writeln!(o, "];\n")?;
        Ok(())
    };
    slot_list(&mut o, "UNI_SLOTS", &b.uni)?;
    writeln!(o, "/// Block-uniform inputs, in `UNI_SLOTS` order.")?;
    writeln!(o, "pub struct Uni {{")?;
    for (cell, kind) in &b.uni {
        writeln!(o, "    pub c{}: {},", cell, uni_ty(kind))?;
    }
    writeln!(o, "}}\n")?;

    slot_list(&mut o, "ROW_SLOTS", &b.inputs)?;
    writeln!(o, "/// Per-lane inputs, in `ROW_SLOTS` order.")?;
    writeln!(o, "pub struct RowsIn {{")?;
    for (cell, kind) in &b.inputs {
        writeln!(o, "    pub c{}: {},", cell, row_ty(kind))?;
    }
    writeln!(o, "}}\n")?;

    // Where each per-lane input LIVES in the block we were handed.
    // Resolved once per block, in `bind`, rather than once per 16-lane
    // slice: the path walk is cheap but it is not free, and the answer
    // cannot change between slices of one block.
    writeln!(o, "/// Cell ids for `ROW_SLOTS` in the bound block.")?;
    writeln!(o, "pub struct RowSlots {{")?;
    for (cell, _) in &b.inputs {
        writeln!(o, "    pub c{}: u32,", cell)?;
    }
    writeln!(o, "}}\n")?;

    writeln!(
        o,
        "/// Bind to a block by PATH. `None` means this block does not\n\
         /// have a slot the kernel needs, or holds it at the wrong kind -\n\
         /// either way the block takes the interpreter path.\n\
         pub fn bind(b: &Rt2) -> Option<(Uni, RowSlots)> {{\n\
         \x20   let cell = |p: &str| resolve_path(b, p).ok();\n\
         \x20   let u = Uni {{"
    )?;
    for (cell, kind) in &b.uni {
        let path = in_path[cell].as_str();
        let pat = match *kind {
            "num" => "Col::U(AV::Num(n)) => *n",
            "bool" => "Col::U(AV::Bool(v)) => *v",
            other => anyhow::bail!("uniform input kind {}", other),
        };
        writeln!(
            o,
            "        c{c}: match &b.cols[cell({p:?})? as usize] {{ {pat}, _ => return None }},",
            c = cell,
            p = path,
            pat = pat
        )?;
    }
    writeln!(o, "    }};")?;
    writeln!(o, "    let s = RowSlots {{")?;
    for (cell, _) in &b.inputs {
        writeln!(o, "        c{}: cell({:?})?,", cell, in_path[cell].as_str())?;
    }
    writeln!(o, "    }};")?;
    writeln!(o, "    Some((u, s))\n}}\n")?;

    writeln!(
        o,
        "/// Gather rows [lo, lo+16) into lane arrays. Short slices pad by\n\
         /// repeating the last row; padded lanes are ignored by width.\n\
         pub fn rows(b: &Rt2, s: &RowSlots, lo: usize) -> Option<RowsIn> {{\n\
         \x20   let at = |i: usize| -> usize {{ (lo + i).min(b.width - 1) }};\n\
         \x20   Some(RowsIn {{"
    )?;
    for (cell, kind) in &b.inputs {
        match *kind {
            "num" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::N(v) => core::array::from_fn(|i| v[at(i)]),\n\
                 \x20           Col::U(AV::Num(n)) => [*n; W],\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            "bool" => writeln!(
                o,
                "        c{c}: match &b.cols[s.c{c} as usize] {{\n\
                 \x20           Col::V(v) => {{\n\
                 \x20               let mut m = 0u16;\n\
                 \x20               for i in 0..W {{\n\
                 \x20                   match v[at(i)] {{\n\
                 \x20                       AV::Bool(true) => m |= 1 << i,\n\
                 \x20                       AV::Bool(false) => {{}}\n\
                 \x20                       _ => return None,\n\
                 \x20                   }}\n\
                 \x20               }}\n\
                 \x20               m\n\
                 \x20           }}\n\
                 \x20           Col::U(AV::Bool(t)) => if *t {{ 0xffff }} else {{ 0 }},\n\
                 \x20           _ => return None,\n\
                 \x20       }},",
                c = cell
            )?,
            other => anyhow::bail!("row input kind {}", other),
        }
    }
    writeln!(o, "    }})\n}}\n")?;

    // ---- one output shape per outcome ----
    for (i, out) in l.outs.iter().enumerate() {
        let paths: BTreeMap<u32, String> = f.outs[i]
            .fields
            .iter()
            .zip(f.outs[i].cells.iter())
            .map(|((p, _, _), c)| (*c, super::iface::show(p)))
            .collect();
        writeln!(o, "// ---------------- outcome {} ----------------", i)?;
        writeln!(o, "/// (cell, path) - where each computed value goes.")?;
        writeln!(o, "pub const OUT_SLOTS_{}: &[(u32, &str)] = &[", i)?;
        for OutField { cell, .. } in &out.fields {
            writeln!(o, "    ({}, {:?}),", cell, paths[cell].as_str())?;
        }
        writeln!(o, "];\n")?;
        writeln!(
            o,
            "/// Cells that end the frame as a fresh UnknownBool - next\n\
             /// frame's button inputs. The boundary writes UBool, no data."
        )?;
        writeln!(o, "pub const OUT_UBOOL_{}: &[(u32, &str)] = &[", i)?;
        for (c, p) in f.outs[i].ubool_cells.iter().zip(f.outs[i].ubool.iter()) {
            writeln!(o, "    ({}, {:?}),", c, super::iface::show(p).as_str())?;
        }
        writeln!(o, "];\n")?;

        // THE OUTPUT SHAPE, as generated data. Everything the shape
        // determines - structure, globals, pointer columns - is a
        // compile-time constant of this outcome, so the kernel builds
        // its block rather than patching a copy of the input's.
        let rt2 = &f.outs[i].rt2;
        writeln!(o, "pub const OUT_SHAPE_{}: &[SCell] = &[", i)?;
        for cell in &rt2.structure {
            match cell {
                Cell2::Val => writeln!(o, "    SCell::Val,")?,
                Cell2::Obj(fields) => {
                    let items: Vec<String> =
                        fields.iter().map(|(k, c)| format!("({}, {})", k, c)).collect();
                    writeln!(o, "    SCell::Obj(&[{}]),", items.join(", "))?
                }
                Cell2::Arr(items) => {
                    let items: Vec<String> = items.iter().map(|c| c.to_string()).collect();
                    writeln!(o, "    SCell::Arr(&[{}]),", items.join(", "))?
                }
                Cell2::Clo(..) => writeln!(o, "    SCell::Clo,")?,
                other => anyhow::bail!("outcome {} has a {:?} cell, which has no static form", i, other),
            }
        }
        writeln!(o, "];\n")?;
        writeln!(o, "pub const OUT_GLOBALS_{}: &[u32] = &{:?};\n", i, rt2.globals)?;
        writeln!(o, "/// (cell, target) - the pointer topology, fixed by the shape.")?;
        writeln!(o, "pub const OUT_PTRS_{}: &[(u32, u32)] = &[", i)?;
        for (c, col) in rt2.cols.iter().enumerate() {
            if let Col::U(AV::Ptr(t)) = col {
                writeln!(o, "    ({}, {}),", c, t)?;
            }
        }
        writeln!(o, "];\n")?;

        writeln!(o, "/// Outcome {}'s button-INDEPENDENT values.", i)?;
        writeln!(o, "pub struct KShared{} {{", i)?;
        for OutField { cell, ty, tainted, .. } in &out.fields {
            if !*tainted {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(o, "}}\n")?;
        writeln!(o, "/// Outcome {}'s per-assignment values and lane masks.", i)?;
        writeln!(o, "pub struct KOut{} {{", i)?;
        writeln!(o, "    /// Lanes that take THIS successor.")?;
        writeln!(o, "    pub live: u16,")?;
        writeln!(o, "    /// Lanes the kernel declines - they go to the interpreter.")?;
        writeln!(o, "    pub deopt: u16,")?;
        writeln!(o, "    /// The whole block is undecidable here.")?;
        writeln!(o, "    pub bd: bool,")?;
        for OutField { cell, ty, tainted, .. } in &out.fields {
            if *tainted {
                writeln!(o, "    pub c{}: {},", cell, ty)?;
            }
        }
        writeln!(o, "}}\n")?;
    }

    // ---- building an outcome's block, and filling it ----
    for (i, out) in l.outs.iter().enumerate() {
        writeln!(
            o,
            "/// An EMPTY accumulator with outcome {i}'s shape: structure,\n\
             /// globals and pointers from the constants above, computed\n\
             /// columns starting empty and growing by `append{i}`.\n\
             pub fn acc{i}(cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2 {{\n\
             \x20   let mut b = build_block(OUT_SHAPE_{i}, OUT_GLOBALS_{i}, OUT_PTRS_{i}, 0, cart, cache);",
            i = i
        )?;
        for OutField { cell, ty, .. } in &out.fields {
            let empty = match *ty {
                "ZN" => "Col::N(Vec::new())",
                "ZB" => "Col::V(Vec::new())",
                other => anyhow::bail!("output type {}", other),
            };
            writeln!(o, "    b.cols[{}] = {};", cell, empty)?;
        }
        writeln!(
            o,
            "    for (cell, _) in OUT_UBOOL_{} {{\n\
             \x20       b.cols[*cell as usize] = Col::U(AV::UBool);\n\
             \x20   }}\n\
             \x20   b\n\
             }}\n",
            i
        )?;

        writeln!(
            o,
            "/// Append this assignment's lanes that TAKE outcome {i} and\n\
             /// that the kernel is willing to keep. A lane in `deopt` is\n\
             /// dropped here and belongs to the interpreter - the caller\n\
             /// has `kv.deopt` and must account for it.\n\
             pub fn append{i}(acc: &mut Rt2, sh: &KShared{i}, kv: &KOut{i}, n: usize) {{\n\
             \x20   let take = kv.live & !kv.deopt;\n\
             \x20   for i in 0..n {{\n\
             \x20       if take & (1 << i) == 0 {{ continue; }}",
            i = i
        )?;
        for OutField { cell, ty, tainted, .. } in &out.fields {
            let src = if *tainted { "kv" } else { "sh" };
            match *ty {
                "ZN" => writeln!(
                    o,
                    "        if let Col::N(v) = &mut acc.cols[{c}] {{ v.push({s}.c{c}[i]); }}",
                    c = cell,
                    s = src
                )?,
                // An UNKNOWN boolean is a real abstract value, not a
                // broken one, so it goes in as `UBool` rather than being
                // coerced to a definite bit.
                "ZB" => writeln!(
                    o,
                    "        if let Col::V(v) = &mut acc.cols[{c}] {{\n\
                     \x20           v.push(if {s}.c{c}.known & (1 << i) != 0 {{\n\
                     \x20               AV::Bool({s}.c{c}.val & (1 << i) != 0)\n\
                     \x20           }} else {{ AV::UBool }});\n\
                     \x20       }}",
                    c = cell,
                    s = src
                )?,
                other => anyhow::bail!("output type {}", other),
            }
        }
        writeln!(o, "        acc.width += 1;\n    }}\n}}\n")?;
    }

    // One struct so `frame` has a fixed arity whatever the outcome count.
    writeln!(o, "/// Every outcome's result for one button assignment.")?;
    writeln!(o, "pub struct KOuts<'a> {{")?;
    for i in 0..l.outs.len() {
        writeln!(o, "    pub sh{i}: &'a KShared{i},", i = i)?;
        writeln!(o, "    pub v{i}: &'a KOut{i},", i = i)?;
    }
    writeln!(o, "}}\n")?;

    // A small dynamic layer over the per-outcome items above. Rust
    // cannot index a struct or a function name by a runtime `usize`, and
    // a caller that loops over outcomes - the dispatcher, and the check
    // harness - needs to. Generated rather than written by hand so it
    // cannot fall out of step with the outcome count.
    writeln!(o, "pub const OUTCOMES: usize = {};\n", l.outs.len())?;
    let dispatch = |o: &mut String, sig: &str, arm: &str| -> Result<()> {
        writeln!(o, "{} {{", sig)?;
        writeln!(o, "    match i {{")?;
        for k in 0..l.outs.len() {
            writeln!(o, "        {} => {},", k, arm.replace('#', &k.to_string()))?;
        }
        writeln!(o, "        _ => panic!(\"outcome {{}} of {}\", i),", l.outs.len())?;
        writeln!(o, "    }}\n}}\n")?;
        Ok(())
    };
    dispatch(
        &mut o,
        "pub fn acc(i: usize, cart: Arc<CartData>, cache: Arc<CollisionCache>) -> Rt2",
        "acc#(cart, cache)",
    )?;
    dispatch(
        &mut o,
        "pub fn append(i: usize, a: &mut Rt2, o: &KOuts, n: usize)",
        "append#(a, o.sh#, o.v#, n)",
    )?;
    dispatch(&mut o, "pub fn live(i: usize, o: &KOuts) -> u16", "o.v#.live")?;
    dispatch(&mut o, "pub fn deopt(i: usize, o: &KOuts) -> u16", "o.v#.deopt")?;
    dispatch(&mut o, "pub fn bd(i: usize, o: &KOuts) -> bool", "o.v#.bd")?;
    dispatch(&mut o, "pub fn out_slots(i: usize) -> &'static [(u32, &'static str)]", "OUT_SLOTS_#")?;

    // ---- the body ----
    writeln!(
        o,
        "#[inline(never)]\n\
         pub fn frame(u: &Uni, rin: &RowsIn, g: &G, out: &mut impl FnMut(u8, &KOuts)) {{"
    )?;
    o.push_str(&render_lines(&l.body));
    for (i, out) in l.outs.iter().enumerate() {
        writeln!(o, "    let sh{} = KShared{} {{", i, i)?;
        for OutField { cell, expr, tainted, .. } in &out.fields {
            if !*tainted {
                writeln!(o, "        c{}: {},", cell, expr)?;
            }
        }
        writeln!(o, "    }};")?;
    }
    writeln!(
        o,
        "    // {} of 64 button assignments are distinct successors",
        l.variants.len()
    )?;
    for v in &l.variants {
        for (i, out) in l.outs.iter().enumerate() {
            let p = &v.per[i];
            writeln!(o, "    let o{} = KOut{} {{", i, i)?;
            writeln!(o, "        live: {},", p.live)?;
            writeln!(o, "        deopt: !{},", p.ok)?;
            writeln!(o, "        bd: {},", p.bd)?;
            for OutField { cell, tainted, .. } in &out.fields {
                if *tainted {
                    writeln!(o, "        c{}: {},", cell, p.outputs[cell])?;
                }
            }
            writeln!(o, "    }};")?;
        }
        let args: Vec<String> = (0..l.outs.len())
            .map(|i| format!("sh{i}: &sh{i}, v{i}: &o{i}", i = i))
            .collect();
        writeln!(o, "    out({}, &KOuts {{ {} }});", v.mask, args.join(", "))?;
    }
    writeln!(o, "}}")?;
    Ok(o)
}

/// One traced frame, everything owned, ready to render or to run.
///
/// The tracer's `Interp` borrows the parsed ASTs, so a caller outside
/// this crate cannot easily set one up. Nothing in here borrows them -
/// `Frame`, `Graph` and `Rt2` are all owned - so the whole thing can be
/// handed across a crate boundary, which is what the out-of-tree check
/// harness needs.
pub struct Reference {
    pub frame: Frame,
    pub graph: crate::transpile::graph::Graph,
    pub bound: Bound,
    pub lowered: Lowered,
    pub cart: std::sync::Arc<celeste_core::cart_data::CartData>,
    pub cache: std::sync::Arc<celeste_core::collision_cache::CollisionCache>,
}

/// Trace the frame this kernel is rendered from: room (0,0), warmed up
/// to the first frame that has a player, pinned to its pm1 key.
pub fn reference_frame() -> Result<Reference> {
    reference_frame_in(std::path::Path::new("."))
}

/// As `reference_frame`, with the repo root given explicitly - the run
/// check is a crate outside the workspace, so its working directory is
/// not the repo root.
pub fn reference_frame_in(root: &std::path::Path) -> Result<Reference> {
    use super::domain::Symbolic;
    use super::interp::Interp;
    use super::verify::{pm1_key, run_one, trace_frame};
    use super::{cart, iface};
    use anyhow::{anyhow, bail};

    let src = cart::sources_in(root)?;
    let top = full_moon::parse(&src).map_err(|e| anyhow!("parse: {:?}", e))?;
    let init = full_moon::parse("_init()").map_err(|e| anyhow!("parse _init: {:?}", e))?;
    let reset = full_moon::parse("__reset_button_states()")
        .map_err(|e| anyhow!("parse reset: {:?}", e))?;
    let fr = full_moon::parse("_update()").map_err(|e| anyhow!("parse frame: {:?}", e))?;

    let mut it: Interp<Symbolic> = Interp::new(Symbolic::default());
    let cart_data = std::sync::Arc::new(celeste_core::cart_data::CartData::load(root.join("cart"))?);
    let (rx, ry) = celeste_interp::game_runner::start_room();
    let cache = std::sync::Arc::new(celeste_core::collision_cache::CollisionCache::new(
        &cart_data, rx, ry,
    )?);
    it.cache = Some(cache.clone());
    it.cart = Some(cart_data.clone());

    let st = cart::fresh_state::<Symbolic>(&mut it.d);
    let mut st = run_one(&mut it, &top, st)?;
    cart::inject_tile_flag_at(&mut st);
    let mut st = run_one(&mut it, &init, st)?;
    let mut player: Option<super::iface::Path> = None;
    for _ in 0..40 {
        if let Some(p) = super::verify::find_player(&st) {
            player = Some(p);
            break;
        }
        st = run_one(&mut it, &fr, st)?;
    }
    let Some(player) = player else { bail!("no player after 40 frames") };

    let mut roots = vec![player.clone()];
    for g in ["freeze", "has_dashed", "frames", "will_restart", "delay_restart", "max_djump"] {
        roots.push(vec![iface::key(g)]);
    }
    let pin = pm1_key(&player, &st, &it.d)?;
    let frame = trace_frame(&mut it, &reset, &fr, st, &roots, &pin)?;

    let graph = std::mem::take(&mut it.d.graph);
    let bound = super::emit::bind(&frame, &graph)?;
    let room = crate::transpile::graph::Room { cart: cart_data.clone(), cache: cache.clone() };
    let lowered = super::emit::lower_frame(
        &bound.graph,
        &bound.inputs,
        &bound.uni,
        &bound.outcomes,
        Some(room),
    )?;
    Ok(Reference { frame, graph, bound, lowered, cart: cart_data, cache })
}

/// A block holding `rows` concrete input assignments, one per lane.
///
/// The kernel reads its inputs off a block by path, so checking it
/// against the graph needs a block that holds exactly the values the
/// graph is evaluated at. This builds one: the traced input shape, with
/// each row's values written into the cells the interface named.
pub fn input_block(r: &Reference, rows: &[Vec<super::iface::Conc>]) -> Result<celeste_engine::Rt2> {
    use super::iface::Conc;

    let mut b = celeste_engine::slots::reshape(&r.frame.in_rt2, rows.len());
    // The BLOCK-UNIFORM inputs go in as uniform columns, because that is
    // what the kernel's `bind` accepts - it takes them as `P8`, not as a
    // lane array, and a per-lane column there is a narrowing it refuses.
    // Every row therefore has to agree about them, and this says so
    // rather than silently taking row 0.
    let uni: std::collections::BTreeSet<u32> = r.bound.uni.iter().map(|(c, _)| *c).collect();
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if !uni.contains(cell) {
            continue;
        }
        for (k, row) in rows.iter().enumerate() {
            if row[i] != rows[0][i] {
                anyhow::bail!(
                    "row {} disagrees with row 0 about the block-uniform slot {}",
                    k,
                    super::iface::show(&r.frame.iface.slots[i])
                );
            }
        }
        b.cols[*cell as usize] = Col::U(match rows[0][i] {
            Conc::Num(v) => AV::Num(v),
            Conc::Bool(v) => AV::Bool(v),
        });
    }
    for (i, cell) in r.frame.in_cells.iter().enumerate() {
        if uni.contains(cell) {
            continue;
        }
        let col = match rows[0].get(i) {
            Some(Conc::Num(_)) => Col::N(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Num(v) => v,
                        Conc::Bool(_) => celeste_core::pico8_num::Pico8Num::from_i16(0),
                    })
                    .collect(),
            ),
            Some(Conc::Bool(_)) => Col::V(
                rows.iter()
                    .map(|row| match row[i] {
                        Conc::Bool(v) => AV::Bool(v),
                        Conc::Num(_) => AV::Nil,
                    })
                    .collect(),
            ),
            None => anyhow::bail!("row is shorter than the interface"),
        };
        b.cols[*cell as usize] = col;
    }
    Ok(b)
}
