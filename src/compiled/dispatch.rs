//! Kernel DISPATCH: which class kernel, if any, runs a chunk.
//!
//! The registry is three generated class kernels (steady / dash / frozen)
//! tried in coverage order. Trying in order is sound because each kernel
//! guards its own class - a wrong-class chunk fails the kernel's own `gb`
//! check on slice 0 and costs one slice, not a frame. A chunk every kernel
//! refuses returns `false` and takes the interpreter reference path, so a
//! gap in coverage is SLOW and never wrong.
//!
//! This module was the probe's; it moved into celeste-rust with the rest of
//! the frame interface (task #150) so the campaign can reach it. What is
//! left in it is dispatch and the diagnostic counters - the frame itself is
//! `super::FrameEngine::step`.

use celeste_engine::runtime2;
use celeste_engine::kernel;
use celeste_kernels::{kernel_gen_dash, kernel_gen_frozen, kernel_gen_steady};

pub(crate) fn run_chunk_kernel(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<runtime2::Rt2>,
    local: &mut Vec<(runtime2::Rt2, bool)>,
) -> bool {
    // The class registry: each kernel's own guards reject wrong-class
    // chunks (bd on the first slice - cheap), so trying in coverage
    // order is both sound and fast.
    // CELESTE_KERNEL_CLASSES=steady,dash (default: all) - diagnostic
    // knob for bisecting a class kernel against the reference path.
    let mask = kernel_class_mask();
    // The fused specialization-set kernel covers steady AND dying lanes in
    // one pass (feature `fused`, artifact generated per campaign); it runs
    // ahead of the steady class kernel because it strictly extends it.
    #[cfg(feature = "fused")]
    if fused::enabled() && fused::run_fused(chunk, ids, done, local) {
        fused::LANES.fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    if mask & 1 != 0 && run_class_kernel_steady(chunk, ids, done, local) {
        KERNEL_HITS[0].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    if mask & 2 != 0 && run_class_kernel_dash(chunk, ids, done, local) {
        KERNEL_HITS[1].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    if mask & 4 != 0 && run_class_kernel_frozen(chunk, ids, done, local) {
        KERNEL_HITS[2].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    KERNEL_HITS[3].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
    // Why did every kernel refuse? A shape hash that matches some kernel
    // means the CLASS guard rejected it (a pm1 the overlays do not
    // cover); no match at all means the heap shape itself is new. The
    // two want completely different work, so record which.
    if std::env::var("CELESTE_KERNEL_MISS").is_ok() {
        let known = [
            ("steady", kernel_gen_steady::SHAPE_HASH),
            ("dash", kernel_gen_dash::SHAPE_HASH),
            ("frozen", kernel_gen_frozen::SHAPE_HASH),
        ];
        let same_shape: Vec<&str> = known
            .iter()
            .filter(|(_, h)| *h == chunk.shape_hash)
            .map(|(n, _)| *n)
            .collect();
        let mut miss = KERNEL_MISS.lock().unwrap();
        *miss.entry((chunk.shape_hash, same_shape.join("/"))).or_insert(0u64) +=
            chunk.width as u64;
    }
    false
}

/// Missed chunks by (shape hash, which kernels share that shape).
static KERNEL_MISS: std::sync::Mutex<std::collections::BTreeMap<(u64, String), u64>> =
    std::sync::Mutex::new(std::collections::BTreeMap::new());

/// Where a class kernel refused a chunk, by (class, step). Diagnostic
/// only (CELESTE_KERNEL_MISS=1): "shape" is a different heap, "bind" a
/// uniform/kind mismatch, "rows" a per-lane kind mismatch, "guard" the
/// kernel's own class or premise check.
static KERNEL_MISS_WHY: std::sync::Mutex<
    std::collections::BTreeMap<(&'static str, &'static str), u64>,
> = std::sync::Mutex::new(std::collections::BTreeMap::new());

/// Which block-uniform input a kernel could not bind, by cell and by the
/// column kind that was there instead. Diagnostic only.
static BIND_FAIL: std::sync::Mutex<std::collections::BTreeMap<(u32, &'static str), u64>> =
    std::sync::Mutex::new(std::collections::BTreeMap::new());

fn note_bind_failure(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    uni_cells: &[(u32, &str)],
) {
    if std::env::var("CELESTE_KERNEL_MISS").is_err() {
        return;
    }
    // One-shot detail on the first refusal: which cells vary, with the
    // distinct values, so a "this cell varies" line can be traced back
    // to a partition that should have split it.
    static ONCE: std::sync::Once = std::sync::Once::new();
    ONCE.call_once(|| {
        eprintln!("kernel miss detail: chunk width {}", chunk.width);
        for (cell, want) in uni_cells {
            let vals: Vec<String> = match &chunk.cols[*cell as usize] {
                runtime2::Col::N(vs) => {
                    let mut d: Vec<String> =
                        vs.iter().map(|n| format!("{:?}", n)).collect();
                    d.sort();
                    d.dedup();
                    d
                }
                runtime2::Col::V(vs) => {
                    let mut d: Vec<String> = vs.iter().map(|v| format!("{:?}", v)).collect();
                    d.sort();
                    d.dedup();
                    d
                }
                _ => continue,
            };
            eprintln!(
                "  cell {} (want {}) has {} distinct values: {}",
                cell,
                want,
                vals.len(),
                vals.iter().take(4).cloned().collect::<Vec<_>>().join(", ")
            );
        }
        eprintln!("  pm1 cells: {:?}", chunk.pm1_cells(ids));
    });
    let mut fail = BIND_FAIL.lock().unwrap();
    *fail
        .entry((
            chunk.player_objects(ids).len() as u32,
            "players; pm1 cells below",
        ))
        .or_insert(0) += chunk.width as u64;
    *fail
        .entry((chunk.pm1_cells(ids).len() as u32, "pm1 cells"))
        .or_insert(0) += chunk.width as u64;
    for (cell, want) in uni_cells {
        let got: &'static str = match &chunk.cols[*cell as usize] {
            runtime2::Col::U(runtime2::AV::Num(_)) => "U(num)",
            runtime2::Col::U(runtime2::AV::Bool(_)) => "U(bool)",
            runtime2::Col::U(runtime2::AV::Ival(..)) => "U(ival)",
            runtime2::Col::U(runtime2::AV::UBool) => "U(ubool)",
            runtime2::Col::U(_) => "U(other)",
            runtime2::Col::N(_) => "N(varying)",
            runtime2::Col::I(_) => "I(varying)",
            runtime2::Col::V(_) => "V(varying)",
        };
        let ok = matches!((*want, got), ("num", "U(num)") | ("bool", "U(bool)"));
        if !ok {
            *fail.entry((*cell, got)).or_insert(0) += chunk.width as u64;
        }
    }
}

fn note_miss(class: &'static str, step: &'static str, lanes: usize) {
    if std::env::var("CELESTE_KERNEL_MISS").is_err() {
        return;
    }
    *KERNEL_MISS_WHY.lock().unwrap().entry((class, step)).or_insert(0) += lanes as u64;
}

/// Pre-dedup rows out of the kernel's registers before materializing
/// them (plans/dedup-on-the-fly-plan.md). `CELESTE_PREDEDUP=0` restores
/// the materialize-everything path for A/B measurement.
fn prededup_on() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("CELESTE_PREDEDUP").map(|v| v != "0").unwrap_or(true))
}

/// Which boundary canonicalizations apply to this chunk's key cells.
/// `mark_walk` answers it from the heap walk; the kernel only knows cell
/// ids. Computed on the INPUT block and re-checked against the OUTPUT
/// block before the boundary, because skipping a cell that is NOT a rem
/// cell downstream would merge rows that differ.
fn key_plan(
    b: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    key_cells: &[u32],
) -> kernel::KeyPlan {
    let (rem, det) = b.mark_walk(ids);
    let mut plan = kernel::KeyPlan::default();
    for (j, c) in key_cells.iter().enumerate() {
        if rem.contains(c) {
            plan.rem |= 1 << j;
        }
        if det.contains(c) {
            plan.det |= 1 << j;
        }
    }
    plan
}

/// Which class kernels are enabled (bit 0 steady, 1 dash, 2 frozen).
/// Default all; `CELESTE_KERNEL_CLASSES=steady,frozen` restricts, and
/// the disabled classes fall through to the reference path.
fn kernel_class_mask() -> u8 {
    use std::sync::atomic::{AtomicU8, Ordering};
    static MASK: AtomicU8 = AtomicU8::new(0xff);
    let m = MASK.load(Ordering::Relaxed);
    if m != 0xff {
        return m;
    }
    let m = match std::env::var("CELESTE_KERNEL_CLASSES") {
        Ok(v) => v.split(',').fold(0u8, |acc, s| {
            acc | match s.trim() {
                "steady" => 1,
                "dash" => 2,
                "frozen" => 4,
                "" => 0,
                other => panic!("unknown kernel class {:?}", other),
            }
        }),
        Err(_) => 7,
    };
    MASK.store(m, Ordering::Relaxed);
    m
}

/// Lanes handled per class kernel [steady, dash, frozen, missed].
static KERNEL_HITS: [std::sync::atomic::AtomicU64; 4] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Kernel rows [materialized by append_out, surviving within-chunk dedup].
static KROWS: [std::sync::atomic::AtomicU64; 2] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

pub fn print_kernel_hits() {
    #[cfg(feature = "fused")]
    fused::print_stats();
    let v: Vec<u64> = KERNEL_HITS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    if v.iter().any(|x| *x > 0) {
        eprintln!(
            "kernel lanes: steady {} dash {} frozen {} missed {}",
            v[0], v[1], v[2], v[3]
        );
    }
    {
        let mut miss = KERNEL_MISS.lock().unwrap();
        for ((hash, same), lanes) in miss.iter() {
            eprintln!(
                "kernel miss: shape {:#x} lanes {} ({})",
                hash,
                lanes,
                if same.is_empty() { "new shape - no kernel has it" } else { same }
            );
        }
        miss.clear();
        let mut why = KERNEL_MISS_WHY.lock().unwrap();
        for ((class, step), lanes) in why.iter() {
            eprintln!("kernel miss: {} refused at {} ({} lanes)", class, step, lanes);
        }
        why.clear();
        let mut fail = BIND_FAIL.lock().unwrap();
        for ((cell, got), lanes) in fail.iter() {
            eprintln!("kernel miss: cell {} is {} ({} lanes)", cell, got, lanes);
        }
        fail.clear();
    }
    let rows: Vec<u64> = KROWS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    if rows[0] > 0 {
        eprintln!(
            "kernel rows: materialized {} -> {} after within-chunk dedup ({:.1}:1)",
            rows[0],
            rows[1],
            rows[0] as f64 / rows[1].max(1) as f64
        );
    }
}

macro_rules! class_kernel_runner {
    ($fname:ident, $m:ident) => {
fn $fname(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<runtime2::Rt2>,
    local: &mut Vec<(runtime2::Rt2, bool)>,
) -> bool {
    if chunk.shape_hash != $m::SHAPE_HASH {
        note_miss(stringify!($m), "shape", chunk.width);
        return false;
    }
    let Some(uni) = $m::bind(chunk) else {
        note_miss(stringify!($m), "bind", chunk.width);
        note_bind_failure(chunk, ids, $m::UNI_CELLS);
        return false;
    };
    let g = $m::G { cart: &chunk.cart, cache: &chunk.cache };
    let mut acc = $m::acc_init(chunk);
    let plan = key_plan(chunk, ids, $m::KEY_CELLS);
    // Pre-dedup: a chunk emits one row per (lane, fork config, button
    // variant), and at f35 8.3 of every 9 of those are duplicates that
    // boundary would throw away AFTER they were materialized and hashed.
    // Key them straight out of the kernel's output registers instead and
    // materialize only the first occurrence.
    let mut seen: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
    let mut keys = [(0u64, 0u64); kernel::W];
    let mut deopt_rows: std::collections::BTreeSet<u32> = Default::default();
    let mut bd_hit = false;
    let mut lo = 0usize;
    while lo < chunk.width && !bd_hit {
        let n = (chunk.width - lo).min(kernel::W);
        let width_mask: u16 = if n == kernel::W { 0xffff } else { (1u16 << n) - 1 };
        let Some(rin) = $m::rows(chunk, lo) else {
            note_miss(stringify!($m), "rows", chunk.width);
            return false;
        };
        $m::frame(&uni, &rin, &g, &mut |_b, osh, kout| {
            if kout.bd {
                bd_hit = true;
                return;
            }
            let dead = kout.deopt & kout.valid & width_mask;
            for i in 0..n {
                if dead & (1 << i) != 0 {
                    deopt_rows.insert((lo + i) as u32);
                }
            }
            let mut live = kout.valid & !kout.deopt & width_mask;
            if live == 0 {
                return;
            }
            if prededup_on() {
                $m::row_keys(chunk, lo, n, osh, kout, &plan, &mut keys);
                for i in 0..n {
                    if live & (1 << i) != 0 && !seen.insert(keys[i]) {
                        live &= !(1 << i);
                    }
                }
                if live == 0 {
                    return;
                }
            }
            let mut ug = false; // uniform-output cross-config guard
            $m::append_out(&mut acc, chunk, lo, n, live, osh, kout, &mut ug);
            if ug {
                bd_hit = true;
            }
        });
        lo += kernel::W;
    }
    if bd_hit {
        // A guard inside the kernel refused: wrong class, or a uniform
        // premise that did not hold.
        note_miss(stringify!($m), "guard", chunk.width);
        return false;
    }
    if acc.width > 0 {
        // Dedup-census (plans/dedup-on-the-fly-plan.md): how much of the
        // 45:1 duplication is reachable WITHIN a chunk? That is the
        // ceiling for deduping before materializing.
        let before = acc.width as u64;
        // The key plan was read off the INPUT block; hold that it still
        // describes the OUTPUT block, since a stale rem bit would merge
        // rows that boundary keeps apart.
        let out_plan = key_plan(&acc, ids, $m::KEY_CELLS);
        assert!(
            out_plan.rem == plan.rem && out_plan.det == plan.det,
            "key plan changed across the frame: in {:?} out {:?}",
            plan,
            out_plan
        );
        acc.boundary(ids);
        KROWS[0].fetch_add(before, std::sync::atomic::Ordering::Relaxed);
        KROWS[1].fetch_add(acc.width as u64, std::sync::atomic::Ordering::Relaxed);
        done.push(acc);
    }
    if !deopt_rows.is_empty() {
        let keep: Vec<u32> = deopt_rows.iter().copied().collect();
        let mut sub = chunk.clone_block();
        sub.retain_lanes(&keep);
        local.push((sub, false));
    }
    true
}
    };
}
class_kernel_runner!(run_class_kernel_steady, kernel_gen_steady);
class_kernel_runner!(run_class_kernel_dash, kernel_gen_dash);
class_kernel_runner!(run_class_kernel_frozen, kernel_gen_frozen);

/// The fused specialization-set runner (plans/shape-tag-plan.md). Same
/// slice loop as the class-kernel macro above, with one addition: the
/// fused frame reports, per (config, variant), which lanes each DYING
/// member covers plus that member's block-uniform boundary tuple. Covered
/// dying lanes are provably interchangeable per tuple (their boundary
/// rows are identical), so the runner keeps ONE representative lane per
/// distinct tuple and routes only representatives through the interpreter
/// deopt path - the row SET is preserved exactly while the deopt
/// population drops from every dying lane to a handful per chunk. Lanes
/// no member covers still deopt individually, loudly counted.
#[cfg(feature = "fused")]
mod fused {
    use super::*;
    use celeste_kernels::fused_gen_player as fg;
    use std::sync::atomic::{AtomicU64, Ordering};

    pub(super) fn enabled() -> bool {
        static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
        *ON.get_or_init(|| std::env::var("CELESTE_FUSED").map(|v| v != "0").unwrap_or(true))
    }

    /// Lanes handled by the fused kernel.
    pub(super) static LANES: AtomicU64 = AtomicU64::new(0);
    /// [dying-covered lane events, representatives pushed, uncovered lane events]
    static DY_STATS: [AtomicU64; 3] =
        [AtomicU64::new(0), AtomicU64::new(0), AtomicU64::new(0)];
    /// Uncovered lane events by button variant `B` - which kb bits the
    /// residual coverage gap is structured on.
    static DY_UNCOV_BY_B: [AtomicU64; 64] = [const { AtomicU64::new(0) }; 64];

    pub(super) fn print_stats() {
        let lanes = LANES.swap(0, Ordering::Relaxed);
        let v: Vec<u64> = DY_STATS.iter().map(|a| a.swap(0, Ordering::Relaxed)).collect();
        if lanes > 0 || v.iter().any(|x| *x > 0) {
            eprintln!(
                "fused: lanes {} dying-covered-events {} reps {} UNCOVERED-events {}",
                lanes, v[0], v[1], v[2]
            );
        }
        let hist: Vec<u64> =
            DY_UNCOV_BY_B.iter().map(|a| a.swap(0, Ordering::Relaxed)).collect();
        if hist.iter().any(|x| *x > 0) {
            // Aggregate per kb bit: events in variants with the bit set.
            for k in 0..6 {
                let with: u64 =
                    (0..64).filter(|b| b >> k & 1 == 1).map(|b| hist[b]).sum();
                let without: u64 =
                    (0..64).filter(|b| b >> k & 1 == 0).map(|b| hist[b]).sum();
                eprintln!("fused uncovered by kb{}: set {} clear {}", k, with, without);
            }
        }
    }

    pub(super) fn run_fused(
        chunk: &runtime2::Rt2,
        ids: &runtime2::BoundaryIds,
        done: &mut Vec<runtime2::Rt2>,
        local: &mut Vec<(runtime2::Rt2, bool)>,
    ) -> bool {
        if chunk.shape_hash != fg::SHAPE_HASH {
            note_miss("fused", "shape", chunk.width);
            return false;
        }
        let Some(uni) = fg::bind(chunk) else {
            note_miss("fused", "bind", chunk.width);
            note_bind_failure(chunk, ids, fg::UNI_CELLS);
            return false;
        };
        let g = fg::G { cart: &chunk.cart, cache: &chunk.cache };
        let mut acc = fg::acc_init(chunk);
        let plan = key_plan(chunk, ids, fg::KEY_CELLS);
        let mut seen: rustc_hash::FxHashSet<(u64, u64)> = Default::default();
        let mut keys = [(0u64, 0u64); kernel::W];
        let mut deopt_rows: std::collections::BTreeSet<u32> = Default::default();
        // tuple raw-bits key -> representative lane (chunk index).
        let mut dy_reps: std::collections::BTreeMap<_, u32> = Default::default();
        let mut dy_covered: u64 = 0;
        let mut uncovered: u64 = 0;
        let mut bd_hit = false;
        let mut lo = 0usize;
        while lo < chunk.width && !bd_hit {
            let n = (chunk.width - lo).min(kernel::W);
            let width_mask: u16 = if n == kernel::W { 0xffff } else { (1u16 << n) - 1 };
            let Some(rin) = fg::rows(chunk, lo) else {
                note_miss("fused", "rows", chunk.width);
                return false;
            };
            fg::frame(&uni, &rin, &g, &mut |b, osh, kout, dy| {
                if kout.bd {
                    bd_hit = true;
                    return;
                }
                let mut covered_any: u16 = 0;
                for m in 0..fg::N_DY {
                    covered_any |= dy.covered[m];
                }
                // Lanes NO member covers: the interpreter runs them whole.
                let dead = kout.deopt & !covered_any & kout.valid & width_mask;
                if dead != 0 {
                    uncovered += dead.count_ones() as u64;
                    DY_UNCOV_BY_B[b as usize]
                        .fetch_add(dead.count_ones() as u64, Ordering::Relaxed);
                    for i in 0..n {
                        if dead & (1 << i) != 0 {
                            deopt_rows.insert((lo + i) as u32);
                        }
                    }
                }
                // Dying members: one representative per distinct
                // (tuple, per-lane vary) key. The vary part reads boundary
                // cells the fuse pass proved are the primary's own out
                // columns (e.g. the corpse dash-start freeze/has_dashed).
                for m in 0..fg::N_DY {
                    let mask = dy.covered[m] & width_mask;
                    if mask == 0 {
                        continue;
                    }
                    dy_covered += mask.count_ones() as u64;
                    let tup = dy.tuples[m].key();
                    for i in 0..n {
                        if mask & (1 << i) != 0 {
                            dy_reps
                                .entry((tup, fg::dy_vary_key(osh, kout, i)))
                                .or_insert((lo + i) as u32);
                        }
                    }
                }
                // The primary (steady) member: exactly the class path.
                let mut live = kout.valid & !kout.deopt & width_mask;
                if live == 0 {
                    return;
                }
                if prededup_on() {
                    fg::row_keys(chunk, lo, n, osh, kout, &plan, &mut keys);
                    for i in 0..n {
                        if live & (1 << i) != 0 && !seen.insert(keys[i]) {
                            live &= !(1 << i);
                        }
                    }
                    if live == 0 {
                        return;
                    }
                }
                let mut ug = false;
                fg::append_out(&mut acc, chunk, lo, n, live, osh, kout, &mut ug);
                if ug {
                    bd_hit = true;
                }
            });
            lo += kernel::W;
        }
        if bd_hit {
            note_miss("fused", "guard", chunk.width);
            return false;
        }
        if acc.width > 0 {
            let before = acc.width as u64;
            let out_plan = key_plan(&acc, ids, fg::KEY_CELLS);
            assert!(
                out_plan.rem == plan.rem && out_plan.det == plan.det,
                "key plan changed across the frame: in {:?} out {:?}",
                plan,
                out_plan
            );
            acc.boundary(ids);
            KROWS[0].fetch_add(before, std::sync::atomic::Ordering::Relaxed);
            KROWS[1].fetch_add(acc.width as u64, std::sync::atomic::Ordering::Relaxed);
            done.push(acc);
        }
        DY_STATS[0].fetch_add(dy_covered, Ordering::Relaxed);
        DY_STATS[1].fetch_add(dy_reps.len() as u64, Ordering::Relaxed);
        DY_STATS[2].fetch_add(uncovered, Ordering::Relaxed);
        for lane in dy_reps.values() {
            deopt_rows.insert(*lane);
        }
        if !deopt_rows.is_empty() {
            let keep: Vec<u32> = deopt_rows.iter().copied().collect();
            let mut sub = chunk.clone_block();
            sub.retain_lanes(&keep);
            local.push((sub, false));
        }
        true
    }
}

/// A width-`n` copy of lanes [lo, lo+n) of a block (structure shared by
/// clone, varying columns sliced) - the canvas kernel outputs land on.
pub fn slice_block(b: &runtime2::Rt2, lo: usize, n: usize) -> runtime2::Rt2 {
    let mut out = runtime2::Rt2::empty(n, b.globals.len(), &[], b.cart.clone(), b.cache.clone());
    out.strings = b.strings.clone();
    out.globals = b.globals.clone();
    out.structure = b.structure.clone();
    out.cols = b
        .cols
        .iter()
        .map(|c| match c {
            runtime2::Col::U(_) => c.clone(),
            runtime2::Col::N(v) => runtime2::Col::N(v[lo..lo + n].to_vec()),
            runtime2::Col::I(v) => runtime2::Col::I(v[lo..lo + n].to_vec()),
            runtime2::Col::V(v) => runtime2::Col::V(v[lo..lo + n].to_vec()),
        })
        .collect();
    out.shape_hash = b.shape_hash;
    out
}
