#![cfg(feature = "lattice")]
//! Gate 2 for the constant-lattice kernels (plans/specialize.md): run
//! room (2,0) on the lattice set ALONE and confirm it (a) covers every
//! reachable shape and (b) never declines a block (bd never fires) - the
//! lattice's soundness. With ROOM2_ORACLE set, also compare per-frame
//! row-key SETS to the interpreter.
use std::collections::{BTreeMap, BTreeSet};
use traced_kernel_check::lattice;
use celeste_engine::runtime2::{Rt2, Cell2, AV, Col, NONE};
use celeste_names::FIELD_NAMES;

/// Flat map of every SCALAR leaf field of lane 0, keyed by dotted path.
/// Follows pointers (visited-guarded) and records only scalars, so shared
/// type tables cancel between two blocks and the diff isolates the field
/// that actually differs.
fn scalar_fields(b: &Rt2) -> BTreeMap<String, String> { scalar_fields_lane(b, 0) }
fn scalar_fields_lane(b: &Rt2, lane: usize) -> BTreeMap<String, String> {
    fn nm(id: u32) -> String {
        FIELD_NAMES.get(id as usize).map(|s| s.to_string()).unwrap_or_else(|| format!("#{}", id))
    }
    fn walk(b: &Rt2, cell: u32, path: &str, depth: u32, lane: usize, out: &mut BTreeMap<String, String>) {
        if cell == NONE || depth > 16 { return; }
        match &b.structure[cell as usize] {
            Cell2::Val => match b.cols[cell as usize].at(lane) {
                AV::Ptr(t) => walk(b, t, path, depth + 1, lane, out),
                AV::NilPtr | AV::Nil => {}
                scalar => { out.insert(path.to_string(), format!("{:?}", scalar)); }
            },
            Cell2::Obj(fields) => {
                for (fid, c) in fields { walk(b, *c, &format!("{}.{}", path, nm(*fid)), depth + 1, lane, out); }
            }
            Cell2::Arr(items) => {
                for (i, it) in items.iter().enumerate() { walk(b, *it, &format!("{}[{}]", path, i), depth + 1, lane, out); }
            }
            _ => {}
        }
    }
    let mut out = BTreeMap::new();
    for gi in 0..b.globals.len() {
        let cell = b.globals[gi];
        if cell != NONE { walk(b, cell, &nm(gi as u32), 0, lane, &mut out); }
    }
    out
}

/// Reachable non-pointer scalar multiset - the "ignore dead cells / closure
/// boxing" state signature. Two heaps with identical game state agree here
/// even if they box closure upvalues differently.
fn scalar_sig(b: &Rt2) -> BTreeMap<String, i32> {
    let mut m: BTreeMap<String, i32> = BTreeMap::new();
    for i in 0..b.structure.len() {
        if matches!(b.structure[i], Cell2::Val) {
            let v = b.cols[i].at(0);
            if !matches!(v, AV::Ptr(_) | AV::NilPtr) { *m.entry(format!("{:?}", v)).or_default() += 1; }
        }
    }
    m
}

fn diff_report(lattice: &Rt2, interp: &Rt2) -> String {
    let nm = |id: u32| FIELD_NAMES.get(id as usize).copied().unwrap_or("?");
    let kinds = |b: &Rt2| {
        let (mut v,mut o,mut a,mut c,mut u,mut bi)=(0,0,0,0,0,0);
        for cell in &b.structure { match cell {
            Cell2::Val=>v+=1, Cell2::Obj(_)=>o+=1, Cell2::Arr(_)=>a+=1,
            Cell2::Clo(..)=>c+=1, Cell2::Unk=>u+=1, Cell2::Bi(_)=>bi+=1 } }
        format!("Val={} Obj={} Arr={} Clo={} Unk={} Bi={}", v,o,a,c,u,bi)
    };
    let mut s = format!("\nKINDS lattice: {}\nKINDS interp:  {}\n", kinds(lattice), kinds(interp));

    // Path-based reachable scalar fields (ignores cell layout / dead pointers).
    let (a, b) = (scalar_fields(lattice), scalar_fields(interp));
    let mut keys: BTreeSet<&String> = a.keys().collect(); keys.extend(b.keys());
    s.push_str("field-path diffs (reachable scalars):\n");
    let mut fd = 0;
    for k in keys { if a.get(k) != b.get(k) { s.push_str(&format!("  {} | {:?} | {:?}\n", k, a.get(k), b.get(k))); fd+=1; } }
    s.push_str(&format!("  ({} field-path diffs)\n", fd));

    // Reachable NON-POINTER scalar multiset - the user's "ignore dead cells" check.
    let scal = |b: &Rt2| { let mut m: BTreeMap<String,i32>=BTreeMap::new();
        for i in 0..b.structure.len() { if matches!(b.structure[i],Cell2::Val) {
            let v=b.cols[i].at(0); if !matches!(v, AV::Ptr(_)|AV::NilPtr) { *m.entry(format!("{:?}",v)).or_default()+=1; } } } m };
    let (ls, is_) = (scal(lattice), scal(interp));
    let mut ks: BTreeSet<&String> = ls.keys().collect(); ks.extend(is_.keys());
    s.push_str("non-pointer scalar multiset deltas:\n");
    let mut sd = 0;
    for k in ks { let (x,y)=(*ls.get(k).unwrap_or(&0),*is_.get(k).unwrap_or(&0)); if x!=y { s.push_str(&format!("  {}: L={} I={}\n",k,x,y)); sd+=1; } }
    s.push_str(&format!("  ({} scalar-value deltas)\n", sd));

    // Every reference to each object, to name the redundant ref.
    let trace_refs = |b: &Rt2, target: u32| -> Vec<String> {
        let mut hits = Vec::new();
        for g in 0..b.globals.len() { let c=b.globals[g]; if c==NONE {continue;}
            let hit = match &b.structure[c as usize] { Cell2::Val => b.cols[c as usize].at(0)==AV::Ptr(target), _=> c==target };
            if hit { hits.push(format!("global#{}'{}'", g, nm(g as u32))); } }
        for j in 0..b.structure.len() { match &b.structure[j] {
            Cell2::Obj(fields)=>{ for (fid,cc) in fields { if *cc==target || (matches!(b.structure[*cc as usize],Cell2::Val)&&b.cols[*cc as usize].at(0)==AV::Ptr(target)) { hits.push(format!("Obj[{}].{}",j,nm(*fid))); } } }
            Cell2::Arr(items)=>{ for (k,cc) in items.iter().enumerate() { if *cc==target || (matches!(b.structure[*cc as usize],Cell2::Val)&&b.cols[*cc as usize].at(0)==AV::Ptr(target)) { hits.push(format!("Arr[{}][{}]",j,k)); } } }
            Cell2::Clo(cid,caps)=>{ for cap in caps.iter() { if matches!(cap, Col::U(AV::Ptr(t)) if *t==target) { hits.push(format!("Clo{}[{}]",cid,j)); } } }
            _=>{} } }
        hits
    };
    for obj in [207u32,208,209,210] {
        s.push_str(&format!("obj{} L:{:?}  I:{:?}\n", obj, trace_refs(lattice,obj), trace_refs(interp,obj)));
    }
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

    let mut t_kernel = std::time::Duration::ZERO;
    let mut t_interp = std::time::Duration::ZERO;
    for frame in 1..=frames {
        let __t0 = std::time::Instant::now();
        let st = run.step().unwrap_or_else(|e| panic!("lattice frame {}: {:#}\n(a declined block or an uncovered shape means the lattice over-claimed a constant)", frame, e));
        t_kernel += __t0.elapsed();
        eprintln!("[r2-lattice] frame {:>3}: {:>7} in -> {:>7} out, {} blocks", st.frame, st.rows_in, st.rows_out, st.blocks_out);
        if let (Some(oracle), Some(engine)) = (oracle.as_mut(), engine.as_ref()) {
            let __t1 = std::time::Instant::now();
            oracle.step().unwrap_or_else(|e| panic!("oracle frame {}: {:#}", frame, e));
            t_interp += __t1.elapsed();
            // Row-key equality is confounded by closure-upvalue BOXING (the
            // compiled interp boxes each object in an extra cell; the AST
            // tracer captures it directly). So compare the REACHABLE state
            // ignoring that: per-row when single-row, non-pointer scalar
            // multiset + field-path (Philippe's "same ignoring dead cells").
            let row_key_match = {
                let want: BTreeSet<(u64,u64)> = st.keys.iter().copied().collect();
                let got: BTreeSet<(u64,u64)> = engine.row_key_set(oracle.states()).into_iter().collect();
                want == got
            };
            // Set of per-lane reachable states (field-path -> value), which is
            // lane-order and closure-boxing independent - Philippe's "same
            // ignoring dead cells", generalized to multi-row frames.
            let sig_of = |b: &Rt2| -> BTreeSet<Vec<(String,String)>> {
                (0..b.width).map(|l| scalar_fields_lane(b, l).into_iter().collect::<Vec<_>>()).collect()
            };
            let mut lat: BTreeSet<Vec<(String,String)>> = BTreeSet::new();
            for b in run.blocks() { lat.extend(sig_of(b)); }
            let mut inp: BTreeSet<Vec<(String,String)>> = BTreeSet::new();
            for st_ in oracle.states() {
                if st_.vector_size == 0 { continue; }
                let mut ib = celeste_rust::compiled::bridge::import_block(st_, run_cart.clone(), run_cache.clone());
                ib.boundary(engine.ids());
                inp.extend(sig_of(&ib));
            }
            if lat != inp {
                let only_lat = lat.difference(&inp).count();
                let only_inp = inp.difference(&lat).count();
                panic!("frame {}: reachable STATE-SET differs (not just boxing): {} states lattice-only, {} interp-only (lattice {} states, interp {})",
                    frame, only_lat, only_inp, lat.len(), inp.len());
            }
            eprintln!("[r2-lattice] frame {:>3}: {} reachable states MATCH interpreter (row-key {})",
                frame, lat.len(), if row_key_match { "also equal" } else { "differs only by closure-boxing" });
        }
    }
    eprintln!("[r2-lattice] {} frames clean{}", frames, if oracle_on { " and matching the interpreter" } else { " (coverage + no declines)" });
    eprintln!("[r2-timing] {} kernel frames: {:.3} s total ({:.2} ms/frame)", frames, t_kernel.as_secs_f64(), t_kernel.as_secs_f64()*1000.0/frames as f64);
    if oracle_on { eprintln!("[r2-timing] {} interpreter frames: {:.3} s total ({:.2} ms/frame); kernel is {:.2}x the interpreter's wall", frames, t_interp.as_secs_f64(), t_interp.as_secs_f64()*1000.0/frames as f64, t_interp.as_secs_f64()/t_kernel.as_secs_f64().max(1e-9)); }
}
