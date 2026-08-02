//! Measures how big a fully-inlined, branch-free version of the program would be.
//!
//! The central open question in `plans/rewrite-plan.md` is K: the static
//! instruction count of the frame body once every call is inlined, every loop
//! is unrolled to its worst-case trip count, and every conditional is turned
//! into a `select`. In that form each lane executes *all* of the code, so K
//! decides whether the transformation pays for itself.
//!
//! K cannot be read off the source, because it depends on which code is
//! reachable at all and on real loop bounds (`move_x` iterates `abs(amount)`
//! times, which depends on speed). So we measure it: run the *concrete*
//! interpreter over many input sequences and record, per frame,
//!
//!   - which `(function, block)` pairs execute at all - the union over all
//!     inputs is the code the branch-free kernel must contain, and
//!   - how many times each executes within a single frame - the maximum over
//!     all inputs is the unroll factor that block needs.
//!
//! Then `K = sum over blocks of (instructions * max_executions_in_one_frame)`.
//!
//! This over-estimates slightly (not every block needs its worst-case unroll on
//! every path) and under-estimates slightly (sampled inputs may miss rare
//! code). It is intended as an order-of-magnitude answer, which is all the
//! decision needs.
//!
//! Enabled by setting `CELESTE_BLOCK_COVERAGE=1`; zero cost otherwise (one
//! relaxed atomic load per block execution).

use rustc_hash::FxHashMap;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Mutex;

static ENABLED: AtomicBool = AtomicBool::new(false);

#[derive(Default)]
struct Coverage {
    /// (function, block) -> instruction count of that block
    instructions: FxHashMap<(String, String), usize>,
    /// (function, block) -> how many instructions of each kind it contains
    kinds: FxHashMap<(String, String), Vec<(&'static str, usize)>>,
    /// (function, block) -> executions in the frame currently being recorded
    this_frame: FxHashMap<(String, String), usize>,
    /// (function, block) -> max executions observed in any single frame
    max_per_frame: FxHashMap<(String, String), usize>,
    /// Frames counted, and frames dropped by the exclusion filter.
    frames: usize,
    excluded_frames: usize,
    /// Sum and max of per-frame dynamic instruction counts (what a single lane
    /// actually executes today, as opposed to K).
    dynamic_total: usize,
    dynamic_max: usize,
}

static COVERAGE: Mutex<Option<Coverage>> = Mutex::new(None);

/// Groups instructions into the categories that matter for sizing the kernel.
///
/// `heap` instructions are the ones `promote_cell` is meant to remove entirely
/// once pointers are specialized, so they are the part of K we expect not to
/// survive. `call` disappears under inlining. `arith`/`const`/`select` are the
/// irreducible core that a compiled kernel would actually emit.
fn instruction_kind(instr: &crate::ir::Instruction) -> &'static str {
    use crate::ir::Instruction::*;
    match instr {
        Alloc | Load { .. } | Store { .. } | StoreEmptyTable { .. } | GetField { .. }
        | GetIndex { .. } => "heap",
        GetGlobal { .. } => "global",
        StoreClosure { .. } | Call { .. } => "call",
        NumberConstant { .. } | BoolConstant { .. } | StringConstant { .. } | NilConstant => {
            "const"
        }
        UnaryOp { .. } | BinaryOp { .. } => "arith",
        Phi { .. } => "phi",
    }
}

/// Turn recording on. Call once at startup.
pub fn enable() {
    *COVERAGE.lock().unwrap() = Some(Coverage::default());
    ENABLED.store(true, Ordering::Relaxed);
}

pub fn enabled_via_env() -> bool {
    std::env::var("CELESTE_BLOCK_COVERAGE").is_ok_and(|v| v != "0")
}

#[inline]
pub fn is_enabled() -> bool {
    ENABLED.load(Ordering::Relaxed)
}

/// Record one execution of a block. `function` is the enclosing CFG's name.
pub fn record_block(function: &str, label: &str, block: &crate::ir::Block) {
    if !is_enabled() {
        return;
    }
    let mut guard = COVERAGE.lock().unwrap();
    let Some(cov) = guard.as_mut() else { return };
    let key = (function.to_string(), label.to_string());
    if !cov.instructions.contains_key(&key) {
        cov.instructions.insert(key.clone(), block.instructions.len());
        let mut kinds: FxHashMap<&'static str, usize> = FxHashMap::default();
        for (_, instr) in &block.instructions {
            *kinds.entry(instruction_kind(instr)).or_insert(0) += 1;
        }
        *kinds.entry("terminator").or_insert(0) += 1;
        cov.kinds.insert(key.clone(), kinds.into_iter().collect());
    }
    *cov.this_frame.entry(key).or_insert(0) += 1;
}

/// Close out the current frame, folding its counts into the running maxima.
///
/// Frames in which any block of `exclude_function` ran are dropped entirely.
/// This is how room-load frames are kept out of the steady-state measurement:
/// `load_room` scans 16x16 tiles and runs `foreach(types, ...)` per tile, which
/// is ~2800 block executions and swamps everything else. It also only happens
/// when the object set changes, i.e. at a different heap shape, so it would be
/// a separate specialization anyway.
pub fn end_frame_excluding(exclude_function: Option<&str>) {
    if !is_enabled() {
        return;
    }
    let mut guard = COVERAGE.lock().unwrap();
    let Some(cov) = guard.as_mut() else { return };
    let this_frame = std::mem::take(&mut cov.this_frame);

    if let Some(excluded) = exclude_function {
        if this_frame.keys().any(|(func, _)| func == excluded) {
            cov.excluded_frames += 1;
            return;
        }
    }

    cov.frames += 1;
    let mut dynamic = 0usize;
    for (key, count) in this_frame {
        let instrs = cov.instructions.get(&key).copied().unwrap_or(0) + 1;
        dynamic += instrs * count;
        let entry = cov.max_per_frame.entry(key).or_insert(0);
        *entry = (*entry).max(count);
    }
    cov.dynamic_total += dynamic;
    cov.dynamic_max = cov.dynamic_max.max(dynamic);
}

pub fn end_frame() {
    end_frame_excluding(None);
}

pub struct Report {
    /// Estimated size of the fully inlined, unrolled, branch-free frame body.
    pub k_instructions: usize,
    /// Distinct (function, block) pairs reached at all.
    pub distinct_blocks: usize,
    /// Blocks that ever run more than once in a single frame, worst first:
    /// (function, block, instructions, max_executions).
    pub hot_unrolled: Vec<(String, String, usize, usize)>,
    /// Per-function contribution to K, worst first.
    pub by_function: Vec<(String, usize)>,
    /// Contribution to K by instruction kind, worst first.
    pub by_kind: Vec<(&'static str, usize)>,
    pub frames: usize,
    pub excluded_frames: usize,
    /// Mean instructions a single lane executes per frame today.
    pub mean_dynamic: f64,
    pub max_dynamic: usize,
}

pub fn report() -> Option<Report> {
    let guard = COVERAGE.lock().unwrap();
    let cov = guard.as_ref()?;

    let mut k = 0usize;
    let mut per_function: FxHashMap<String, usize> = FxHashMap::default();
    let mut per_kind: FxHashMap<&'static str, usize> = FxHashMap::default();
    let mut hot = Vec::new();

    for (key, &max_exec) in &cov.max_per_frame {
        let instrs = cov.instructions.get(key).copied().unwrap_or(0);
        // +1 for the terminator, which is also work in the flattened form
        let contribution = (instrs + 1) * max_exec;
        k += contribution;
        *per_function.entry(key.0.clone()).or_insert(0) += contribution;
        if let Some(kinds) = cov.kinds.get(key) {
            for (kind, count) in kinds {
                *per_kind.entry(kind).or_insert(0) += count * max_exec;
            }
        }
        if max_exec > 1 {
            hot.push((key.0.clone(), key.1.clone(), instrs + 1, max_exec));
        }
    }

    hot.sort_by_key(|(_, _, i, e)| std::cmp::Reverse(i * e));
    let mut by_function: Vec<(String, usize)> = per_function.into_iter().collect();
    by_function.sort_by_key(|(_, v)| std::cmp::Reverse(*v));
    let mut by_kind: Vec<(&'static str, usize)> = per_kind.into_iter().collect();
    by_kind.sort_by_key(|(_, v)| std::cmp::Reverse(*v));

    Some(Report {
        k_instructions: k,
        distinct_blocks: cov.max_per_frame.len(),
        hot_unrolled: hot,
        by_function,
        by_kind,
        frames: cov.frames,
        excluded_frames: cov.excluded_frames,
        mean_dynamic: if cov.frames == 0 {
            0.0
        } else {
            cov.dynamic_total as f64 / cov.frames as f64
        },
        max_dynamic: cov.dynamic_max,
    })
}
