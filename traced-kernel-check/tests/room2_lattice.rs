#![cfg(feature = "lattice")]
//! Gate 2 for the constant-lattice kernels (plans/specialize.md): run
//! room (2,0) on the lattice set ALONE and confirm it (a) covers every
//! reachable shape and (b) never declines a block (bd never fires) - the
//! lattice's soundness. With ROOM2_ORACLE set, also compare per-frame
//! row-key SETS to the interpreter.
use std::collections::{BTreeMap, BTreeSet};
use traced_kernel_check::lattice;
use celeste_engine::runtime2::{Rt2, Cell2, AV, NONE};
use celeste_names::FIELD_NAMES;

/// Flat map of every SCALAR leaf field of lane 0, keyed by dotted path.
/// Follows pointers (visited-guarded) and records only scalars, so shared
/// type tables cancel between two blocks and the diff isolates the field
/// that actually differs.
fn scalar_fields(b: &Rt2) -> BTreeMap<String, String> {
    fn nm(id: u32) -> String {
        FIELD_NAMES.get(id as usize).map(|s| s.to_string()).unwrap_or_else(|| format!("#{}", id))
    }
    fn walk(b: &Rt2, cell: u32, path: &str, depth: u32, out: &mut BTreeMap<String, String>) {
        if cell == NONE || depth > 14 { return; }
        match &b.structure[cell as usize] {
            Cell2::Val => match b.cols[cell as usize].at(0) {
                AV::Ptr(t) => walk(b, t, path, depth + 1, out),
                AV::NilPtr | AV::Nil => {}
                scalar => { out.insert(path.to_string(), format!("{:?}", scalar)); }
            },
            Cell2::Obj(fields) => {
                for (fid, c) in fields { walk(b, *c, &format!("{}.{}", path, nm(*fid)), depth + 1, out); }
            }
            Cell2::Arr(items) => {
                for (i, it) in items.iter().enumerate() { walk(b, *it, &format!("{}[{}]", path, i), depth + 1, out); }
            }
            _ => {}
        }
    }
    let mut out = BTreeMap::new();
    for gi in 0..b.globals.len() {
        let cell = b.globals[gi];
        if cell != NONE { walk(b, cell, &nm(gi as u32), 0, &mut out); }
    }
    out
}

fn diff_report(lattice: &Rt2, interp: &Rt2) -> String {
    let (a, b) = (scalar_fields(lattice), scalar_fields(interp));
    let mut keys: BTreeSet<&String> = a.keys().collect();
    keys.extend(b.keys());
    let kinds = |b: &Rt2| {
        let (mut v, mut o, mut a, mut c, mut u, mut bi) = (0,0,0,0,0,0);
        for cell in &b.structure { match cell {
            Cell2::Val=>v+=1, Cell2::Obj(_)=>o+=1, Cell2::Arr(_)=>a+=1,
            Cell2::Clo(..)=>c+=1, Cell2::Unk=>u+=1, Cell2::Bi(_)=>bi+=1 } }
        format!("Val={} Obj={} Arr={} Clo={} Unk={} Bi={}", v,o,a,c,u,bi)
    };
    let mut s = format!("\nKINDS lattice: {}\nKINDS interp:  {}\n", kinds(lattice), kinds(interp));
    for k in keys {
        if a.get(k) != b.get(k) {
            s.push_str(&format!("  {} | {:?} | {:?}\n", k, a.get(k), b.get(k)));
        }
    }
    // Raw cell-by-cell diff (canonical index): this is exactly what the row
    // key hashes, so it catches UBool/Str/order differences the path walk misses.
    s.push_str(&format!("\ncell-diff (len lattice={} interp={}):\n", lattice.structure.len(), interp.structure.len()));
    let n = lattice.structure.len().min(interp.structure.len());
    let mut shown = 0;
    for i in 0..n {
        let (la, ib_) = (&lattice.structure[i], &interp.structure[i]);
        let vl = matches!(la, Cell2::Val).then(|| lattice.cols[i].at(0));
        let vi = matches!(ib_, Cell2::Val).then(|| interp.cols[i].at(0));
        let same_kind = std::mem::discriminant(la) == std::mem::discriminant(ib_);
        if !same_kind || vl != vi {
            if shown < 40 {
                s.push_str(&format!("  [{}] {:?}={:?} | {:?}={:?}\n", i, la, vl, ib_, vi));
            }
            shown += 1;
        }
    }
    s.push_str(&format!("  ...{} cell diffs total\n", shown));
    // Multiset of Val values: the 4 extra interp Vals show as count deltas.
    {
        let mut lm: BTreeMap<String, i32> = BTreeMap::new();
        let mut im: BTreeMap<String, i32> = BTreeMap::new();
        for i in 0..lattice.structure.len() { if matches!(lattice.structure[i], Cell2::Val) { *lm.entry(format!("{:?}", lattice.cols[i].at(0))).or_default() += 1; } }
        for i in 0..interp.structure.len() { if matches!(interp.structure[i], Cell2::Val) { *im.entry(format!("{:?}", interp.cols[i].at(0))).or_default() += 1; } }
        let mut ks: BTreeSet<&String> = lm.keys().collect(); ks.extend(im.keys());
    
    // Which GLOBAL points at cell 358 (the extra ref) in interp? And does lattice have that global?
    {
        let mut msg = String::new();
        for g in 0..interp.globals.len() {
            let c = interp.globals[g];
            if c != NONE {
                let tgt = match &interp.structure[c as usize] { Cell2::Val => if let AV::Ptr(t)=interp.cols[c as usize].at(0) { Some(t) } else { None }, _=>Some(c) };
                if tgt == Some(207) { msg.push_str(&format!("interp global#{} '{}' -> obj207 (cell {})\n", g, FIELD_NAMES.get(g).copied().unwrap_or("?"), c)); }
            }
        }
        for g in 0..lattice.globals.len() {
            let c = lattice.globals[g];
            if c != NONE {
                let tgt = match &lattice.structure[c as usize] { Cell2::Val => if let AV::Ptr(t)=lattice.cols[c as usize].at(0) { Some(t) } else { None }, _=>Some(c) };
                if tgt == Some(207) { msg.push_str(&format!("lattice global#{} '{}' -> obj207 (cell {})\n", g, FIELD_NAMES.get(g).copied().unwrap_or("?"), c)); }
            }
        }
        s.push_str(&msg);
    }
    s.push_str("\n-- Val value multiset deltas (lattice vs interp) --\n");
        for k in ks { let (a,b)=(*lm.get(k).unwrap_or(&0), *im.get(k).unwrap_or(&0)); if a!=b { s.push_str(&format!("  {}: L={} I={}\n", k, a, b)); } }
    }

    // What are the interpreter's extra Ptr targets? Print a few tables.


    s
}

#[test]
fn room2_lattice_runs_and_covers() {
    std::env::set_var("CELESTE_START_ROOM", "2,0");
    std::env::set_current_dir("..").expect("cd to repo root");
    let root = std::path::Path::new(".");
    let frames: usize = std::env::var("ROOM2_FRAMES").ok().and_then(|v| v.parse().ok()).unwrap_or(30);

    let (start, cart, cache) =
        celeste_rust::trace::run::start_block(root).expect("room-2 start block");
    let (run_cart, run_cache) = (cart.clone(), cache.clone());
    let mut run = celeste_rust::trace::run::Run::new(lattice::KERNELS, start, cart, cache)
        .expect("index lattice kernels by shape");

    let oracle_on = std::env::var_os("ROOM2_ORACLE").is_some();
    let (mut oracle, engine) = if oracle_on {
        let program = celeste_rust::program::Program::compile_from_disk().expect("compile room-2 program");
        let engine = celeste_rust::compiled::FrameEngine::new_for_start_room(&program).expect("reference engine");
        let oracle = celeste_rust::search::run::AbstractRun::start(&program).expect("oracle");
        (Some(oracle), Some(engine))
    } else { (None, None) };

    // Frame 0: are the two START states even the same? (start_block runs the
    // tracer AST init; AbstractRun runs the compiled init_cfg.)
    if std::env::var_os("SHOW_START").is_some() { if let (Some(oracle), Some(engine)) = (oracle.as_ref(), engine.as_ref()) {
        let ids = engine.ids();
        let lstart = &run.blocks()[0];
        let istates = oracle.states();
        eprintln!("[r2-lattice] START lattice: {} blocks, fruit={} player={}",
            run.blocks().len(),
            lstart.objects_of_type(ids, ids.g_fruit).len(),
            lstart.player_objects(ids).len());
        if istates.len() == 1 {
            let mut ib = celeste_rust::compiled::bridge::import_block(&istates[0], run_cart.clone(), run_cache.clone());
            ib.boundary(ids);
            eprintln!("[r2-lattice] START interp:  {} states, fruit={} player={}",
                istates.len(), ib.objects_of_type(ids, ids.g_fruit).len(), ib.player_objects(ids).len());
            let d = diff_report(lstart, &ib);
            eprintln!("[r2-lattice] START diff:{}", if d.trim().lines().count() <= 1 { " (identical)".to_string() } else { d });
        } else {
            eprintln!("[r2-lattice] START interp:  {} states", istates.len());
        }
    } }

    for frame in 1..=frames {
        let st = run.step().unwrap_or_else(|e| panic!("lattice frame {}: {:#}\n(a declined block or an uncovered shape means the lattice over-claimed a constant)", frame, e));
        eprintln!("[r2-lattice] frame {:>3}: {:>7} in -> {:>7} out, {} blocks", st.frame, st.rows_in, st.rows_out, st.blocks_out);
        if let (Some(oracle), Some(engine)) = (oracle.as_mut(), engine.as_ref()) {
            oracle.step().unwrap_or_else(|e| panic!("oracle frame {}: {:#}", frame, e));
            let want: BTreeSet<(u64, u64)> = st.keys.iter().copied().collect();
            let got: BTreeSet<(u64, u64)> = engine.row_key_set(oracle.states()).into_iter().collect();
            if want != got {
                // Single-row frames: dump the field-level diff. Bridge the
                // interpreter state to a block the same way row_key_set does.
                let mut extra = String::new();
                let lblocks = run.blocks();
                let istates = oracle.states();
                if lblocks.len() == 1 && istates.len() == 1 {
                    let mut ib = celeste_rust::compiled::bridge::import_block(
                        &istates[0], run_cart.clone(), run_cache.clone());
                    ib.boundary(engine.ids());
                    let ids = engine.ids();
                    extra = format!(
                        "\ncounts  lattice: fruit={} player={}   interp: fruit={} player={}{}",
                        lblocks[0].objects_of_type(ids, ids.g_fruit).len(),
                        lblocks[0].player_objects(ids).len(),
                        ib.objects_of_type(ids, ids.g_fruit).len(),
                        ib.player_objects(ids).len(),
                        diff_report(&lblocks[0], &ib));
                }
                panic!("frame {}: lattice {} rows vs interpreter {} - {} missing, {} extra{}",
                    frame, want.len(), got.len(), got.difference(&want).count(), want.difference(&got).count(), extra);
            }
        }
    }
    eprintln!("[r2-lattice] {} frames clean{}", frames, if oracle_on { " and matching the interpreter" } else { " (coverage + no declines)" });
}
