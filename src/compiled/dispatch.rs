//! Kernel DISPATCH: which kernel, if any, runs a chunk.
//!
//! The registry is the TRACED kernel set (`celeste_kernels::traced`): one
//! kernel per heap SHAPE the start room reaches, indexed by the chunk's
//! shape hash. A chunk no kernel takes returns `false` and takes the
//! interpreter reference path - counted and named, because a coverage gap
//! that only shows up as wall clock is the failure mode this campaign is
//! trying to avoid (CLAUDE.md "Never deopt to the interpreter").
//!
//! This module was the probe's; it moved into celeste-rust with the rest of
//! the frame interface (task #150) so the campaign can reach it. What is
//! left in it is dispatch and the diagnostic counters - the frame itself is
//! `super::FrameEngine::step`.

use celeste_engine::kernel;
use celeste_engine::runtime2;

/// Which traced set runs, and which boundary its accumulators take
/// (plans/kernel-ladder.md).
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub(crate) enum TracedMode {
    /// The checked-in level-0 set (`celeste_kernels::traced`): the
    /// Bits(0) widenings are in the graph, accumulators go through
    /// `Rt2::boundary`. Valid ONLY at rem Bits(0) / spd Exact -
    /// `compiled_forward` refuses anything else.
    Level0,
    /// The rung-agnostic set (`celeste_kernels::ladder`): exact rows,
    /// `Rt2::boundary_exact`; the campaign boundary applies whichever
    /// precision rung is configured. Serves rem Bits(0..=15).
    Level0Agnostic,
}

/// The mode, decided once per process: `CELESTE_TRACED_SET=traced|ladder`
/// overrides; otherwise the rem rung picks (Bits(0) -> the level-0 set,
/// every other rung -> the rung-agnostic set). `compiled_forward`
/// validates the (mode, precision) combination before any chunk runs.
pub(crate) fn traced_mode() -> TracedMode {
    static MODE: std::sync::OnceLock<TracedMode> = std::sync::OnceLock::new();
    *MODE.get_or_init(|| match std::env::var("CELESTE_TRACED_SET").as_deref() {
        Ok("traced") => TracedMode::Level0,
        Ok("ladder") => TracedMode::Level0Agnostic,
        Ok(other) => panic!("CELESTE_TRACED_SET={:?}: expected traced or ladder", other),
        Err(_) => {
            use crate::interpreter::abstraction::RemPrecision;
            match crate::interpreter::abstraction::rem_precision_from_env() {
                RemPrecision::Bits(0) => TracedMode::Level0,
                _ => TracedMode::Level0Agnostic,
            }
        }
    })
}

/// `CELESTE_KERNEL_STRICT=1`: a chunk the kernel set cannot take is a
/// FATAL coverage gap (CLAUDE.md "Never deopt to the interpreter"), not
/// a fall-through to the reference path. Off by default because `check`
/// mode and the differential gates need the reference path to exist.
pub(crate) fn kernel_strict() -> bool {
    static ON: std::sync::OnceLock<bool> = std::sync::OnceLock::new();
    *ON.get_or_init(|| std::env::var("CELESTE_KERNEL_STRICT").is_ok_and(|v| v != "0"))
}

/// Every distinct miss reason with its lane count, for the strict-mode
/// abort. Does NOT clear the census (`print_kernel_hits` does).
pub(crate) fn miss_report() -> String {
    let why = KERNEL_MISS_WHY.lock().unwrap();
    if why.is_empty() {
        return "  (no misses recorded)".to_string();
    }
    why.iter()
        .map(|((class, step), lanes)| {
            format!("  {} refused at {} ({} lanes)", class, step, lanes)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

pub(crate) fn run_chunk_kernel(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<runtime2::Rt2>,
) -> bool {
    if run_traced_kernel(chunk, ids, done) {
        KERNEL_HITS[0].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
        return true;
    }
    KERNEL_HITS[1].fetch_add(chunk.width as u64, std::sync::atomic::Ordering::Relaxed);
    false
}

/// Where the traced set refused a chunk, by step: "shape" is a heap shape
/// no kernel has, "bind" a uniform/kind mismatch, "declined" a lane whose
/// `ok` the kernel could not discharge.
static KERNEL_MISS_WHY: std::sync::Mutex<
    std::collections::BTreeMap<(&'static str, &'static str), u64>,
> = std::sync::Mutex::new(std::collections::BTreeMap::new());

fn note_miss(class: &'static str, step: &'static str, lanes: usize) {
    *KERNEL_MISS_WHY.lock().unwrap().entry((class, step)).or_insert(0) += lanes as u64;
}

/// Lanes the engine routed through the PLAIN program (kernel deopt
/// sub-chunks; see `FrameEngine::plain_block`).
pub(crate) static PLAIN_ROUTED: std::sync::atomic::AtomicU64 =
    std::sync::atomic::AtomicU64::new(0);

/// The traced set's content hash. Hashed into the campaign fingerprint
/// whenever the compiled engine is on: nothing the fingerprint already
/// reads determines which traced kernels ran, so they name themselves.
pub fn traced_set_fingerprint() -> u64 {
    match traced_mode() {
        TracedMode::Level0 => celeste_kernels::traced::FINGERPRINT,
        // Tagged so a rung-agnostic engine never shares checkpoints with
        // a level-0 one, even in the unlikely event the rendered sources
        // hashed equal.
        TracedMode::Level0Agnostic => celeste_kernels::ladder::FINGERPRINT ^ 0x6c61_6464_6572,
    }
}

/// Lanes [0] the traced set ran, [1] missed.
static KERNEL_HITS: [std::sync::atomic::AtomicU64; 2] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Kernel rows [materialized by append_out, surviving within-chunk dedup].
static KROWS: [std::sync::atomic::AtomicU64; 2] = [
    std::sync::atomic::AtomicU64::new(0),
    std::sync::atomic::AtomicU64::new(0),
];

/// Lanes the traced set has run so far, WITHOUT resetting the counter.
///
/// `print_kernel_hits` swaps every counter to zero, so a test that wants
/// to assert the traced kernels actually engaged cannot use it - and an
/// engine differential that passes because nothing ran is the failure
/// this exists to prevent.
pub fn traced_lanes() -> u64 {
    KERNEL_HITS[0].load(std::sync::atomic::Ordering::Relaxed)
}

/// Lanes the traced set has MISSED so far, without resetting the counter.
/// The ladder differential tests assert this is zero: a run where chunks
/// quietly fell through to the reference path would otherwise pass while
/// checking interpreter against interpreter.
pub fn missed_lanes() -> u64 {
    KERNEL_HITS[1].load(std::sync::atomic::Ordering::Relaxed)
}

pub fn print_kernel_hits() {
    let v: Vec<u64> = KERNEL_HITS
        .iter()
        .map(|a| a.swap(0, std::sync::atomic::Ordering::Relaxed))
        .collect();
    let plain = PLAIN_ROUTED.swap(0, std::sync::atomic::Ordering::Relaxed);
    if v.iter().any(|x| *x > 0) || plain > 0 {
        eprintln!(
            "kernel lanes: traced {} missed {} plain-routed {}",
            v[0], v[1], plain
        );
    }
    {
        let mut why = KERNEL_MISS_WHY.lock().unwrap();
        for ((class, step), lanes) in why.iter() {
            eprintln!("kernel miss: {} refused at {} ({} lanes)", class, step, lanes);
        }
        why.clear();
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

/// The shape index over the checked-in traced set, built once.
///
/// `Dispatch::new` REFUSES two kernels for one shape rather than picking
/// by hash order, so a generator bug is a panic here at startup instead
/// of a run that depends on which duplicate a map happened to keep.
fn traced_dispatch() -> &'static crate::trace::dispatch::Dispatch {
    static D: std::sync::OnceLock<crate::trace::dispatch::Dispatch> =
        std::sync::OnceLock::new();
    D.get_or_init(|| {
        let set = match traced_mode() {
            TracedMode::Level0 => celeste_kernels::traced::KERNELS,
            TracedMode::Level0Agnostic => celeste_kernels::ladder::KERNELS,
        };
        crate::trace::dispatch::Dispatch::new(set)
            .expect("the checked-in traced kernel set indexes by shape")
    })
}

fn run_traced_kernel(
    chunk: &runtime2::Rt2,
    ids: &runtime2::BoundaryIds,
    done: &mut Vec<runtime2::Rt2>,
) -> bool {
    // `chunk.shape_hash` is the cached hash the boundary wrote, and every
    // chunk reaching here came off the frontier through one.
    let Some(k) = traced_dispatch().find_by_shape(chunk.shape_hash) else {
        note_miss("traced", "shape", chunk.width);
        return false;
    };
    // One row set per outcome, made once per chunk: the kernel dedups its
    // own output, and allocating the table per slice cost more than the
    // dedup saved (244 MB a frame, trace::run).
    let mut seen: Vec<kernel::RowSet> =
        (0..k.outcomes).map(|_| kernel::RowSet::new()).collect();
    let mut accs: Vec<runtime2::Rt2> = (0..k.outcomes)
        .map(|i| (k.acc)(i, chunk.cart.clone(), chunk.cache.clone()))
        .collect();
    let mut lo = 0usize;
    while lo < chunk.width {
        let n = kernel::W.min(chunk.width - lo);
        let Some(declined) = (k.step)(chunk, lo, n, &mut accs, &mut seen) else {
            note_miss("traced", "bind", chunk.width);
            return false;
        };
        if declined != 0 {
            // A lane whose `ok` the kernel could not discharge. Drop
            // everything and let the chunk take the reference path: the
            // rows already in `accs` are a PREFIX of the answer, and
            // half a chunk in `done` plus the whole chunk again from the
            // reference would double-count it.
            note_miss("traced", "declined", chunk.width);
            return false;
        }
        lo += n;
    }
    for mut acc in accs {
        if acc.width > 0 {
            let before = acc.width as u64;
            match traced_mode() {
                // The level-0 set pre-widened in the graph; the boundary's
                // own widening pass is then a free check that they agree.
                TracedMode::Level0 => {
                    acc.boundary(ids);
                }
                // The rung-agnostic set hands back EXACT rows; widening
                // here would pre-empt the campaign's rung with Bits(0).
                TracedMode::Level0Agnostic => {
                    acc.boundary_exact();
                }
            }
            KROWS[0].fetch_add(before, std::sync::atomic::Ordering::Relaxed);
            KROWS[1].fetch_add(acc.width as u64, std::sync::atomic::Ordering::Relaxed);
            done.push(acc);
        }
    }
    true
}
