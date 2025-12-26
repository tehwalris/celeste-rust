//! Frame dump viewer - displays state summaries from JSONL files

use anyhow::Result;
use clap::{Parser, Subcommand};
use serde::Deserialize;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

#[derive(Debug, Deserialize)]
struct SerializableNum {
    #[allow(dead_code)]
    raw: i32,
    display: String,
}

#[derive(Debug, Deserialize)]
#[serde(tag = "type")]
enum NumOrInterval {
    Number { value: SerializableNum },
    Interval { low: SerializableNum, #[allow(dead_code)] high: SerializableNum },
}

impl NumOrInterval {
    fn display(&self) -> &str {
        match self {
            NumOrInterval::Number { value } => &value.display,
            NumOrInterval::Interval { low, .. } => &low.display, // Just show low for simplicity
        }
    }

    fn as_int(&self) -> Option<i32> {
        match self {
            NumOrInterval::Number { value } => value.display.parse().ok(),
            NumOrInterval::Interval { low, .. } => low.display.parse().ok(),
        }
    }
}

#[derive(Debug, Deserialize)]
struct PlayerSummary {
    x: NumOrInterval,
    y: NumOrInterval,
    spd_x: NumOrInterval,
    spd_y: NumOrInterval,
}

#[derive(Debug, Deserialize)]
struct PlayerSpawnSummary {
    x: NumOrInterval,
    y: NumOrInterval,
    state: NumOrInterval,
    delay: NumOrInterval,
}

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
    state_count: usize,
    expanded_count: usize,
    summaries: Vec<StateSummary>,
}

#[derive(Debug, Deserialize)]
struct FrameDump {
    frame: u32,
    total_states: usize,
    total_expanded: usize,
    groups: Vec<StateGroup>,
}

fn load_frames(path: &str) -> Result<Vec<FrameDump>> {
    let file = File::open(path)?;
    let reader = BufReader::new(file);
    let mut frames = Vec::new();
    for line in reader.lines() {
        let line = line?;
        let frame: FrameDump = serde_json::from_str(&line)?;
        frames.push(frame);
    }
    Ok(frames)
}

fn get_player_positions(frame: &FrameDump) -> Vec<(&PlayerSummary, usize)> {
    let mut positions = Vec::new();
    for group in &frame.groups {
        for summary in &group.summaries {
            if let Some(player) = &summary.player {
                positions.push((player, group.expanded_count / group.state_count));
            }
        }
    }
    positions
}

fn print_overview(frames: &[FrameDump]) {
    for frame in frames {
        let player_count: usize = frame.groups.iter()
            .flat_map(|g| &g.summaries)
            .filter(|s| s.player.is_some())
            .count();

        let spawn_count: usize = frame.groups.iter()
            .flat_map(|g| &g.summaries)
            .filter(|s| s.player_spawn.is_some())
            .count();

        let status = if player_count > 0 {
            format!("player:{}", player_count)
        } else if spawn_count > 0 {
            format!("spawn:{}", spawn_count)
        } else {
            "none".to_string()
        };

        println!(
            "Frame {:>3}: {:>3} states ({:>4} exp) - {}",
            frame.frame, frame.total_states, frame.total_expanded, status
        );
    }
}

fn print_frame_detail(frame: &FrameDump) {
    println!("\n=== Frame {} ===", frame.frame);
    println!("States: {} ({} expanded)", frame.total_states, frame.total_expanded);
    println!("Groups: {}", frame.groups.len());

    let positions = get_player_positions(frame);
    if !positions.is_empty() {
        println!("\nPlayer positions ({}):", positions.len());

        // Group by integer position
        let mut pos_counts: HashMap<(i32, i32), usize> = HashMap::new();
        for (player, _weight) in &positions {
            let x = player.x.as_int().unwrap_or(0);
            let y = player.y.as_int().unwrap_or(0);
            *pos_counts.entry((x, y)).or_default() += 1;
        }

        let mut sorted: Vec<_> = pos_counts.iter().collect();
        sorted.sort_by(|a, b| b.1.cmp(a.1));

        for ((x, y), count) in sorted {
            if *count > 1 {
                println!("  ({}, {}) x{}", x, y, count);
            } else {
                println!("  ({}, {})", x, y);
            }
        }

        // Show speed distribution
        println!("\nSpeed distribution:");
        let mut spd_counts: HashMap<(String, String), usize> = HashMap::new();
        for (player, _) in &positions {
            let key = (player.spd_x.display().to_string(), player.spd_y.display().to_string());
            *spd_counts.entry(key).or_default() += 1;
        }

        let mut sorted: Vec<_> = spd_counts.iter().collect();
        sorted.sort_by(|a, b| b.1.cmp(a.1));

        for ((sx, sy), count) in sorted.iter().take(10) {
            if **count > 1 {
                println!("  spd=({}, {}) x{}", sx, sy, count);
            } else {
                println!("  spd=({}, {})", sx, sy);
            }
        }
    } else {
        // Check for player_spawn
        let spawns: Vec<_> = frame.groups.iter()
            .flat_map(|g| &g.summaries)
            .filter_map(|s| s.player_spawn.as_ref())
            .collect();

        if !spawns.is_empty() {
            println!("\nPlayer spawn ({}):", spawns.len());
            for spawn in spawns.iter().take(5) {
                println!(
                    "  pos=({}, {}) state={} delay={}",
                    spawn.x.display(), spawn.y.display(),
                    spawn.state.display(), spawn.delay.display()
                );
            }
        }
    }
}

fn print_heatmap(frames: &[FrameDump], from_frame: u32, to_frame: Option<u32>) {
    let to_frame = to_frame.unwrap_or(u32::MAX);

    // Collect all x positions
    let mut all_x: Vec<i32> = Vec::new();
    for frame in frames {
        if frame.frame < from_frame || frame.frame > to_frame {
            continue;
        }
        for (player, _) in get_player_positions(frame) {
            if let Some(x) = player.x.as_int() {
                all_x.push(x);
            }
        }
    }

    if all_x.is_empty() {
        println!("No player positions found.");
        return;
    }

    let min_x = *all_x.iter().min().unwrap();
    let max_x = *all_x.iter().max().unwrap();
    let display_max_x = min_x + 60;

    println!("\nHeatmap of player positions (frame vs x-position):\n");

    // Header
    print!("{:>6} | ", "Frame");
    for x in min_x..=display_max_x.min(max_x) {
        print!("{}", x % 10);
    }
    println!();
    println!("{}", "-".repeat(68));

    // Each frame
    for frame in frames {
        if frame.frame < from_frame || frame.frame > to_frame {
            continue;
        }

        let mut x_counts: HashMap<i32, usize> = HashMap::new();
        for (player, _) in get_player_positions(frame) {
            if let Some(x) = player.x.as_int() {
                *x_counts.entry(x).or_default() += 1;
            }
        }

        print!("{:>6} | ", frame.frame);
        for x in min_x..=display_max_x.min(max_x) {
            let count = x_counts.get(&x).copied().unwrap_or(0);
            if count == 0 {
                print!(".");
            } else if count < 10 {
                print!("{}", count);
            } else {
                print!("#");
            }
        }
        println!("  ({} states)", frame.total_states);
    }
}

#[derive(Parser)]
#[command(name = "view_frames")]
#[command(about = "View frame dump JSONL files")]
struct Cli {
    /// Path to JSONL file
    file: String,

    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Show specific frame detail
    Frame {
        /// Frame number to show
        #[arg(short, long)]
        num: u32,
    },
    /// Show position heatmap
    Heatmap {
        /// Start frame
        #[arg(long, default_value_t = 0)]
        from: u32,
        /// End frame
        #[arg(long)]
        to: Option<u32>,
    },
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let frames = load_frames(&cli.file)?;
    println!("Loaded {} frames from {}", frames.len(), cli.file);

    match cli.command {
        Some(Commands::Frame { num }) => {
            if let Some(frame) = frames.iter().find(|f| f.frame == num) {
                print_frame_detail(frame);
            } else {
                println!("Frame {} not found", num);
            }
        }
        Some(Commands::Heatmap { from, to }) => {
            print_heatmap(&frames, from, to);
        }
        None => {
            print_overview(&frames);
        }
    }

    Ok(())
}
