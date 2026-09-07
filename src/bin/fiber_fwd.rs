//! THROWAWAY spike (branch fiber-spike): the FORKED engine (`cengine`)
//! differentially tested against the untouched reference (`trace::refengine`).
//!
//! Each state is kept in the current search's canonical boundary form
//! (make_state_abstract_rem(Bits0) + conservative widenings). Per frame we
//! compute successors two ways and compare the key sets:
//!   * REFERENCE: trace::refengine::RefEngine::run_frame (the oracle).
//!   * MINE:      cengine::fast::engine::FastEngine::run_frame (the fast interpreter being
//!                cut down to a concrete executor with a symbolic rem prior).
//! Symbolic rem goes IN as the interval the cell holds; one straight-line
//! path per leaf, enumerated by re-execution. No sampling.
//! If MINE == REFERENCE every frame, the fork still reproduces the search.
//! Frontier is advanced by the reference (authoritative). Both sides are
//! timed separately (t_ref / t_mine, seconds per frame).
//!
//! Env: MAXFRAMES (default 12).

use anyhow::Result;
use std::collections::HashSet;
use std::time::Instant;

use celeste_rust::cengine::fast::engine::FastEngine as MineEngine;
use celeste_rust::compiled::engine_row_keys;
use celeste_rust::interpreter::abstraction::{
    apply_conservative_widenings, make_state_abstract_rem, win_lane_mask, RemPrecision,
};
use celeste_rust::interpreter::state::State;
use celeste_rust::trace::refengine::RefEngine;

fn canon(st: &State) -> State {
    let mut w = make_state_abstract_rem(st.clone(), RemPrecision::Bits(0));
    w = apply_conservative_widenings(w);
    w.gc();
    w
}

fn key_of(st: &State) -> Result<(u64, u64)> {
    Ok(engine_row_keys(st)?[0])
}

fn main() -> Result<()> {
    std::env::set_var("CELESTE_START_ROOM", "1,0");
    let maxframes: u32 = std::env::var("MAXFRAMES").ok().and_then(|s| s.parse().ok()).unwrap_or(12);
    // NOREF=1: skip the reference entirely and advance by MINE, for timing
    // the fast engine alone (no correctness answer).
    let noref = std::env::var("NOREF").map(|v| v == "1").unwrap_or(false);

    let mut refe = RefEngine::new()?;
    let mut mine = MineEngine::new()?;
    let mut frontier = vec![canon(&refe.initial_state()?)];
    {
        let k_ref = key_of(&frontier[0])?;
        let k_mine = key_of(&canon(&mine.initial_state()?))?;
        println!("initial state: ref {:?} mine {:?} {}", k_ref, k_mine, if k_ref == k_mine { "MATCH" } else { "MISMATCH" });
    }
    let mut visited: HashSet<(u64, u64)> = HashSet::new();
    visited.insert(key_of(&frontier[0])?);

    println!("frame  frontier  ref_succ  mine_succ  matched  ref_only  mine_only    t_ref   t_mine    paths  us/path  evals/path  exec_us/path  leaves  local_keys");
    for frame in 0..maxframes {
        let mut ref_keys: HashSet<(u64, u64)> = HashSet::new();
        let mut ref_new: Vec<State> = Vec::new();
        let mut mine_keys: HashSet<(u64, u64)> = HashSet::new();

        let mut t_ref = 0.0f64;
        let mut t_mine = 0.0f64;
        let mut leaves = 0usize;
        // distinct keys summed per input state: what a perfect fast-side
        // dedup would export
        let mut local_keys = 0usize;
        let ops0 = mine.op_counts();
        let t_exec0 = mine.t_exec;
        let paths0 = mine.paths;
        for st in &frontier {
            if win_lane_mask(st)[0] {
                continue;
            }
            if !noref {
                let t0 = Instant::now();
                let ref_succ = refe.run_frame(st)?;
                t_ref += t0.elapsed().as_secs_f64();
                for succ in ref_succ {
                    let c = canon(&succ);
                    let k = key_of(&c)?;
                    if ref_keys.insert(k) && !visited.contains(&k) {
                        ref_new.push(c);
                    }
                }
            }
            let t0 = Instant::now();
            let mine_succ = mine.run_frame(st)?;
            t_mine += t0.elapsed().as_secs_f64();
            leaves += mine_succ.len();
            let mut local: HashSet<(u64, u64)> = HashSet::new();
            for succ in mine_succ {
                let c = canon(&succ);
                let k = key_of(&c)?;
                local.insert(k);
                if mine_keys.insert(k) && noref && !visited.contains(&k) {
                    ref_new.push(c);
                }
            }
            local_keys += local.len();
        }

        let paths = (mine.paths - paths0) as usize;
        let matched = ref_keys.intersection(&mine_keys).count();
        let ref_only = ref_keys.difference(&mine_keys).count();
        let mine_only = mine_keys.difference(&ref_keys).count();
        println!(
            "{frame:5}  {:8}  {:8}  {:9}  {:7}  {:8}  {:9}  {t_ref:7.2}  {t_mine:7.2}  {paths:7}  {:7.0}  {:10.0}  {:12.0}  {:6}  {:6}",
            frontier.len(),
            ref_keys.len(),
            mine_keys.len(),
            matched,
            ref_only,
            mine_only,
            if paths > 0 { t_mine * 1e6 / paths as f64 } else { 0.0 },
            if paths > 0 { (mine.op_counts().0 - ops0.0) as f64 / paths as f64 } else { 0.0 },
            if paths > 0 { (mine.t_exec - t_exec0) * 1e6 / paths as f64 } else { 0.0 },
            leaves,
            local_keys
        );

        // advance by the reference (authoritative), global-dedup
        let mut next = Vec::new();
        for c in ref_new {
            let k = key_of(&c)?;
            if visited.insert(k) {
                next.push(c);
            }
        }
        frontier = next;
        if frontier.is_empty() {
            eprintln!("frontier empty at frame {frame}");
            break;
        }
    }
    Ok(())
}
