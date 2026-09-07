//! THROWAWAY spike probe (branch fiber-spike): measure, at a real late-frame
//! room-1 checkpoint, how much of the population is genuine interval
//! abstraction vs concrete multiplicity, and how many "states" the proposed
//! concrete-skeleton + (spd,rem)-fiber representation would actually track.
//!
//!   fiber_probe <path-to-fNNN.bin>
//!
//! Reports:
//!   total_rows      - lanes summed over all stored States (current cost)
//!   distinct_full   - distinct engine_row_keys (sanity: ~= total_rows)
//!   distinct_skel   - distinct keys after zeroing spd/rem  == NEW state count
//!   reduction       - total_rows / distinct_skel  (what fibering spd/rem buys)
//!   fiber breadth   - rows-per-skeleton distribution
//!   interval census - how many rem/spd coordinate cells are actually intervals
//!   distinct_pos    - distinct player grid cells (position-only skeleton)

use anyhow::Result;
use std::collections::{HashMap, HashSet};

use celeste_rust::compiled::engine_row_keys;
use celeste_rust::interpreter::abstraction::mark_heap;
use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
use celeste_rust::pico8_num::Pico8Num;
use celeste_rust::search::checkpoint::load_states_from;
use celeste_rust::search::pos_graph::state_cells;

fn main() -> Result<()> {
    std::env::set_var("CELESTE_START_ROOM", "1,0");
    let path = std::env::args().nth(1).expect("usage: fiber_probe <fNNN.bin>");
    eprintln!("loading {path} ...");
    let states = load_states_from(std::path::Path::new(&path))?;
    eprintln!("loaded {} stored State objects", states.len());

    let zero = HeapValue::Value(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(0))));

    let mut total_rows: u64 = 0;
    let mut full_keys: HashSet<(u64, u64)> = HashSet::new();
    let mut skel_breadth: HashMap<(u64, u64), u64> = HashMap::new();
    let mut positions: HashSet<u32> = HashSet::new();

    // interval census: per coordinate field, how many cells are Number (concrete)
    // vs NumberInterval, and total lanes carried.
    let mut rem_iv = 0u64;
    let mut rem_cc = 0u64;
    let mut spd_iv = 0u64;
    let mut spd_cc = 0u64;

    for (i, st) in states.iter().enumerate() {
        total_rows += st.vector_size as u64;

        for k in engine_row_keys(st)? {
            full_keys.insert(k);
        }

        let marks = mark_heap(st);

        // interval accounting on the original state
        for (name, iv, cc) in [
            ("player_rem_xy", &mut rem_iv, &mut rem_cc),
            ("player_spd_xy", &mut spd_iv, &mut spd_cc),
        ] {
            if let Some(ids) = marks.marks.get(name) {
                for &id in ids {
                    match st.heap.get(id) {
                        HeapValue::Value(Value::NumberInterval(_)) => *iv += 1,
                        HeapValue::Value(Value::Number(_)) => *cc += 1,
                        _ => {}
                    }
                }
            }
        }

        // skeleton key: erase spd/rem, then hash
        let mut skel = st.clone();
        for name in ["player_spd_xy", "player_rem_xy"] {
            if let Some(ids) = marks.marks.get(name) {
                for &id in ids {
                    skel.heap.set(id, zero.clone());
                }
            }
        }
        for k in engine_row_keys(&skel)? {
            *skel_breadth.entry(k).or_default() += 1;
        }

        if let Ok(cells) = state_cells(st) {
            positions.extend(cells);
        }

        if i % 200 == 0 {
            eprintln!("  .. {i}/{} states, {total_rows} rows so far", states.len());
        }
    }

    let distinct_skel = skel_breadth.len() as u64;
    let mut breadths: Vec<u64> = skel_breadth.values().copied().collect();
    breadths.sort_unstable();
    let pct = |p: f64| -> u64 {
        if breadths.is_empty() {
            return 0;
        }
        let idx = ((breadths.len() as f64 - 1.0) * p).round() as usize;
        breadths[idx]
    };

    println!("\n================ fiber-spike population probe ================");
    println!("file                : {path}");
    println!("stored State objects: {}", states.len());
    println!("total_rows (lanes)  : {total_rows}");
    println!("distinct_full keys  : {}  (sanity, ~= total_rows)", full_keys.len());
    println!("distinct_skel keys  : {distinct_skel}   <== NEW-approach state count");
    println!(
        "reduction           : {:.2}x   (total_rows / distinct_skel)",
        total_rows as f64 / distinct_skel.max(1) as f64
    );
    println!("distinct positions  : {}  (position-only skeleton)", positions.len());
    println!("--- fiber breadth (rows per skeleton) ---");
    println!(
        "  min {}  p50 {}  p90 {}  p99 {}  max {}  mean {:.2}",
        breadths.first().copied().unwrap_or(0),
        pct(0.50),
        pct(0.90),
        pct(0.99),
        breadths.last().copied().unwrap_or(0),
        total_rows as f64 / distinct_skel.max(1) as f64
    );
    println!("--- interval census (coordinate cells across all states) ---");
    println!("  rem cells: {rem_iv} interval, {rem_cc} concrete");
    println!("  spd cells: {spd_iv} interval, {spd_cc} concrete");
    println!("=============================================================");
    Ok(())
}
