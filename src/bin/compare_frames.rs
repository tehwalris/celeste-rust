//! Compare frame dumps between new interpreter and reference
//!
//! The new interpreter tracks player_spawn during frames 0-23, then player appears.
//! The reference (old hardcoded Rust) tracks player from frame 0.
//! This tool accounts for the 24-frame offset when comparing.

use anyhow::Result;
use clap::Parser;
use serde::Deserialize;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::{BufRead, BufReader};

// Types for new interpreter format
#[derive(Debug, Deserialize)]
struct SerializableNum {
    raw: i32,
    #[allow(dead_code)]
    display: String,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
enum NumOrInterval {
    Number { value: SerializableNum },
    Interval { low: SerializableNum, high: SerializableNum },
}

impl NumOrInterval {
    fn raw(&self) -> i32 {
        match self {
            NumOrInterval::Number { value } => value.raw,
            NumOrInterval::Interval { low, .. } => low.raw,
        }
    }

    fn raw_high(&self) -> i32 {
        match self {
            NumOrInterval::Number { value } => value.raw,
            NumOrInterval::Interval { high, .. } => high.raw,
        }
    }

    fn to_int(&self) -> i32 {
        self.raw() / 65536
    }

    fn is_interval(&self) -> bool {
        matches!(self, NumOrInterval::Interval { .. })
    }

    // Get all integer positions covered by this number/interval
    fn integer_positions(&self) -> Vec<i32> {
        let low = self.raw() / 65536;
        let high = self.raw_high() / 65536;
        (low..=high).collect()
    }
}

#[derive(Debug, Deserialize)]
struct PlayerSummary {
    x: NumOrInterval,
    y: NumOrInterval,
    #[allow(dead_code)]
    spd_x: NumOrInterval,
    #[allow(dead_code)]
    spd_y: NumOrInterval,
}

/// Fields mirror the JSON schema written by `view_frames`; not all are read.
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct PlayerSpawnSummary {
    x: NumOrInterval,
    y: NumOrInterval,
    #[allow(dead_code)]
    state: NumOrInterval,
    #[allow(dead_code)]
    delay: NumOrInterval,
}

/// Fields mirror the JSON schema written by `view_frames`; not all are read.
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct StateSummary {
    #[allow(dead_code)]
    object_count: usize,
    player: Option<PlayerSummary>,
    player_spawn: Option<PlayerSpawnSummary>,
}

#[derive(Debug, Deserialize)]
struct StateGroup {
    #[allow(dead_code)]
    shape_hash: u64,
    #[allow(dead_code)]
    state_count: usize,
    #[allow(dead_code)]
    expanded_count: usize,
    summaries: Vec<StateSummary>,
}

#[derive(Debug, Deserialize)]
struct NewFrameDump {
    frame: u32,
    #[allow(dead_code)]
    total_states: usize,
    #[allow(dead_code)]
    total_expanded: usize,
    groups: Vec<StateGroup>,
}

// Types for reference format
/// Fields mirror the reference JSON schema; not all are read.
#[allow(dead_code)]
#[derive(Debug, Deserialize)]
struct ReferencePosition {
    x: i32,
    y: i32,
    state_count: usize,
    #[serde(default)]
    sample_inputs: Option<Vec<u8>>,
}

#[derive(Debug, Deserialize)]
struct ReferenceFrameDump {
    frame: u32,
    #[allow(dead_code)]
    total_positions: usize,
    #[allow(dead_code)]
    total_states: usize,
    positions: Vec<ReferencePosition>,
}

fn load_new_frames(path: &str) -> Result<Vec<NewFrameDump>> {
    eprintln!("Loading new frames from: {}", path);
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut frames = Vec::new();
    for (i, line) in reader.lines().enumerate() {
        let line = line?;
        eprintln!("Parsing new frame line {}, len={}", i, line.len());
        let frame: NewFrameDump = serde_json::from_str(&line)
            .map_err(|e| anyhow::anyhow!("Error parsing new frame line {}: {} (first 100 chars: {:?})", i, e, &line[..100.min(line.len())]))?;
        frames.push(frame);
    }
    eprintln!("Loaded {} new frames", frames.len());
    Ok(frames)
}

fn load_reference_frames(path: &str) -> Result<Vec<ReferenceFrameDump>> {
    eprintln!("Loading reference frames from: {}", path);
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut frames = Vec::new();
    for (i, line) in reader.lines().enumerate() {
        let line = line?;
        eprintln!("Parsing ref frame line {}, len={}", i, line.len());
        let frame: ReferenceFrameDump = serde_json::from_str(&line)
            .map_err(|e| anyhow::anyhow!("Error parsing ref frame line {}: {} (first 100 chars: {:?})", i, e, &line[..100.min(line.len())]))?;
        frames.push(frame);
    }
    eprintln!("Loaded {} reference frames", frames.len());
    Ok(frames)
}

fn get_new_positions(frame: &NewFrameDump) -> HashSet<(i32, i32)> {
    let mut positions = HashSet::new();
    for group in &frame.groups {
        for summary in &group.summaries {
            if let Some(player) = &summary.player {
                // For intervals, include all integer positions in the range
                for x in player.x.integer_positions() {
                    for y in player.y.integer_positions() {
                        positions.insert((x, y));
                    }
                }
            }
        }
    }
    positions
}

fn get_new_positions_detailed(frame: &NewFrameDump) -> Vec<(i32, i32, bool)> {
    // Returns (x, y, is_interval) for each position
    let mut positions = Vec::new();
    for group in &frame.groups {
        for summary in &group.summaries {
            if let Some(player) = &summary.player {
                let is_interval = player.x.is_interval() || player.y.is_interval();
                positions.push((player.x.to_int(), player.y.to_int(), is_interval));
            }
        }
    }
    positions
}

fn get_reference_positions(frame: &ReferenceFrameDump) -> HashSet<(i32, i32)> {
    frame.positions.iter().map(|p| (p.x, p.y)).collect()
}

fn get_reference_sample_inputs(frame: &ReferenceFrameDump) -> HashMap<(i32, i32), Option<Vec<u8>>> {
    frame.positions.iter().map(|p| ((p.x, p.y), p.sample_inputs.clone())).collect()
}

#[derive(Parser)]
#[command(name = "compare_frames")]
#[command(about = "Compare frame dumps between new interpreter and reference")]
struct Cli {
    /// Path to new interpreter JSONL
    new_file: String,

    /// Path to reference JSONL
    reference_file: String,

    /// Frame offset (reference frame = new frame - offset)
    #[arg(long, default_value_t = 24)]
    offset: i32,

    /// Only show mismatches
    #[arg(long)]
    mismatches_only: bool,

    /// Stop after N mismatches
    #[arg(long)]
    max_mismatches: Option<usize>,
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let new_frames = load_new_frames(&cli.new_file)?;
    let reference_frames = load_reference_frames(&cli.reference_file)?;

    println!("Loaded {} new frames, {} reference frames", new_frames.len(), reference_frames.len());
    println!("Using offset: {} (new frame {} = reference frame 0)\n", cli.offset, cli.offset);

    let ref_map: HashMap<u32, &ReferenceFrameDump> = reference_frames.iter()
        .map(|f| (f.frame, f))
        .collect();

    let mut mismatch_count = 0;
    let mut match_count = 0;
    let mut skip_count = 0;

    for new_frame in &new_frames {
        let ref_frame_num = new_frame.frame as i32 - cli.offset;
        if ref_frame_num < 0 {
            skip_count += 1;
            continue;
        }

        let ref_frame = match ref_map.get(&(ref_frame_num as u32)) {
            Some(f) => f,
            None => {
                if !cli.mismatches_only {
                    println!("Frame {}: no reference (ref frame {})", new_frame.frame, ref_frame_num);
                }
                skip_count += 1;
                continue;
            }
        };

        let new_positions = get_new_positions(new_frame);
        let ref_positions = get_reference_positions(ref_frame);

        let only_in_new: Vec<_> = new_positions.difference(&ref_positions).collect();
        let only_in_ref: Vec<_> = ref_positions.difference(&new_positions).collect();
        let in_both = new_positions.intersection(&ref_positions).count();

        if only_in_new.is_empty() && only_in_ref.is_empty() {
            match_count += 1;
            if !cli.mismatches_only {
                println!("Frame {} (ref {}): OK - {} positions match",
                    new_frame.frame, ref_frame_num, in_both);
            }
        } else {
            mismatch_count += 1;
            println!("\n=== Frame {} (ref {}) MISMATCH ===", new_frame.frame, ref_frame_num);
            println!("Common positions: {}", in_both);

            if !only_in_new.is_empty() {
                println!("Only in NEW ({}):", only_in_new.len());
                let mut sorted: Vec<_> = only_in_new.iter().collect();
                sorted.sort();
                for (x, y) in sorted.iter().take(20) {
                    println!("  ({}, {})", x, y);
                }
                if sorted.len() > 20 {
                    println!("  ... and {} more", sorted.len() - 20);
                }
            }

            if !only_in_ref.is_empty() {
                let sample_inputs = get_reference_sample_inputs(ref_frame);
                println!("Only in REFERENCE ({}):", only_in_ref.len());
                let mut sorted: Vec<_> = only_in_ref.iter().collect();
                sorted.sort();
                for (x, y) in sorted.iter().take(20) {
                    if let Some(Some(inputs)) = sample_inputs.get(&(*x, *y)) {
                        println!("  ({}, {}) sample_inputs={:?}", x, y, inputs);
                    } else {
                        println!("  ({}, {})", x, y);
                    }
                }
                if sorted.len() > 20 {
                    println!("  ... and {} more", sorted.len() - 20);
                }
            }

            // Show if new has intervals
            let detailed = get_new_positions_detailed(new_frame);
            let interval_count = detailed.iter().filter(|(_, _, is_int)| *is_int).count();
            if interval_count > 0 {
                println!("Note: new frame has {} interval positions", interval_count);
            }

            if let Some(max) = cli.max_mismatches {
                if mismatch_count >= max {
                    println!("\nStopping after {} mismatches", max);
                    break;
                }
            }
        }
    }

    println!("\n=== Summary ===");
    println!("Matched:    {}", match_count);
    println!("Mismatched: {}", mismatch_count);
    println!("Skipped:    {}", skip_count);

    if mismatch_count > 0 {
        std::process::exit(1);
    }

    Ok(())
}
