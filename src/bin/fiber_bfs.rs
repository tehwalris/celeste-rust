//! THROWAWAY spike (branch fiber-spike): exact concrete forward BFS on room 1,
//! grouped by concrete skeleton (state minus spd/rem), reporting how the
//! (spd,rem) FIBER per skeleton grows frame by frame. Unlike the level-0
//! checkpoint (rem pre-widened), this keeps rem EXACT, so it measures the
//! fragmentation number directly for as many frames as it stays tractable.
//!
//! Reuses only the low-level concrete interpreter (`ConcreteEngine`).
//! Env: MAXFRAMES (default 40), CAP visited states (default 3,000,000).

use anyhow::Result;
use std::collections::{HashMap, HashSet};

use celeste_rust::compiled::engine_row_keys;
use celeste_rust::concrete::ConcreteEngine;
use celeste_rust::interpreter::abstraction::{mark_heap, win_lane_mask};
use celeste_rust::interpreter::state::State;
use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
use celeste_rust::pico8_num::Pico8Num;

fn full_key(st: &State) -> Result<(u64, u64)> {
    Ok(engine_row_keys(st)?[0])
}

/// Key of the concrete skeleton: everything EXCEPT spd/rem.
fn skel_key(st: &State, zero: &HeapValue) -> Result<(u64, u64)> {
    let marks = mark_heap(st);
    let mut s = st.clone();
    for name in ["player_spd_xy", "player_rem_xy"] {
        if let Some(ids) = marks.marks.get(name) {
            for &id in ids {
                s.heap.set(id, zero.clone());
            }
        }
    }
    Ok(engine_row_keys(&s)?[0])
}

fn main() -> Result<()> {
    std::env::set_var("CELESTE_START_ROOM", "1,0");
    let maxframes: u32 = std::env::var("MAXFRAMES").ok().and_then(|s| s.parse().ok()).unwrap_or(40);
    let cap: usize = std::env::var("CAP").ok().and_then(|s| s.parse().ok()).unwrap_or(3_000_000);
    let zero = HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(0))));

    let mut eng = ConcreteEngine::new()?;
    let init = eng.initial_state()?;
    let mut visited: HashSet<(u64, u64)> = HashSet::new();
    visited.insert(full_key(&init)?);
    let mut frontier = vec![init];

    println!("frame  frontier  skeletons  fib_p50  fib_p99  fib_max  visited_total");
    for frame in 0..=maxframes {
        // fiber-breadth stats over the current frontier
        let mut breadth: HashMap<(u64, u64), u64> = HashMap::new();
        for st in &frontier {
            *breadth.entry(skel_key(st, &zero)?).or_default() += 1;
        }
        let mut bv: Vec<u64> = breadth.values().copied().collect();
        bv.sort_unstable();
        let pk = |p: f64| -> u64 {
            if bv.is_empty() {
                0
            } else {
                bv[(((bv.len() - 1) as f64) * p).round() as usize]
            }
        };
        println!(
            "{frame:5}  {:8}  {:9}  {:7}  {:7}  {:7}  {}",
            frontier.len(),
            breadth.len(),
            pk(0.50),
            pk(0.99),
            bv.last().copied().unwrap_or(0),
            visited.len()
        );

        if frame == maxframes {
            break;
        }

        // expand: 64 button inputs per state, dedup by full concrete key
        let mut next = Vec::new();
        for st in &frontier {
            if win_lane_mask(st)[0] {
                continue; // this state has left room 1
            }
            for byte in 0u8..64 {
                let out = eng.step_frame(st.clone(), byte)?;
                if visited.insert(full_key(&out)?) {
                    next.push(out);
                }
            }
        }
        frontier = next;
        if visited.len() > cap {
            eprintln!("cap {cap} exceeded at frame {frame}; stopping");
            break;
        }
        if frontier.is_empty() {
            eprintln!("frontier empty at frame {frame}; stopping");
            break;
        }
    }
    Ok(())
}
