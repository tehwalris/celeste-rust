//! Measures K: the size a fully inlined, unrolled, branch-free frame body would have.
//!
//! See `src/block_coverage.rs` for the method and its caveats, and
//! `plans/rewrite-plan.md` for why the number matters.
//!
//! Runs the concrete (single-lane) interpreter over many random input
//! sequences, recording which blocks execute and how often within one frame.
//!
//!     cargo run --release --bin measure_k -- --sequences 300 --frames 45

use anyhow::Result;
use celeste_rust::block_coverage;
use celeste_rust::interpreter::state::State;
use celeste_rust::interpreter::value::{HeapValue, MaybeVector, Value};
use clap::Parser;

#[derive(Parser)]
#[command(about = "Measure the size of a branch-free frame body")]
struct Cli {
    /// Number of random input sequences to sample
    #[arg(long, default_value_t = 200)]
    sequences: usize,

    /// Frames to run per sequence
    #[arg(long, default_value_t = 45)]
    frames: u32,

    /// Seed for the input generator
    #[arg(long, default_value_t = 1)]
    seed: u64,

    /// Recipe to replay before measuring. K is a property of the program we
    /// are actually building, so this defaults to the real one; the point of
    /// the rewrites is to drive it down and that is only visible if they are
    /// applied.
    #[arg(long, default_value = "rewrites.jsonl")]
    recipe: String,

    /// Measure the freshly compiled program instead, ignoring the recipe.
    /// The baseline the recipe is trying to improve on.
    #[arg(long)]
    original: bool,

    /// Print every (function, block) pair reached, with its worst-case
    /// executions in one frame. The ground truth for "does this site run":
    /// a rewrite whose guard sits on a never-executed site passes every
    /// screen without testing anything.
    #[arg(long)]
    blocks: bool,
}

/// xorshift64* - we only need cheap reproducible noise, not statistical quality.
struct Rng(u64);
impl Rng {
    fn next(&mut self) -> u64 {
        let mut x = self.0;
        x ^= x >> 12;
        x ^= x << 25;
        x ^= x >> 27;
        self.0 = x;
        x.wrapping_mul(0x2545F4914F6CDD1D)
    }
    fn below(&mut self, n: u64) -> u64 {
        self.next() % n
    }
}

/// The room this state is in, if it can be read.
///
/// Frames in which the room is (re)loaded are a different heap shape and a
/// different specialization; measuring them together is meaningless. Which
/// frames those are is read from the state rather than from which function ran,
/// because inlining renames and eventually erases the function.
fn current_room(state: &State) -> Option<(i16, i16)> {
    let deref = |id| match state.heap.get(id) {
        HeapValue::Value(Value::Pointer(inner)) => Some(*inner),
        HeapValue::ObjectTable(_) => Some(id),
        _ => None,
    };
    let room = deref(*state.global_env.get("room")?)?;
    let HeapValue::ObjectTable(fields) = state.heap.get(room) else { return None };
    let (x, y) = (*fields.get("x")?, *fields.get("y")?);
    let number = |id| match state.heap.get(id) {
        HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => n.as_i16(),
        _ => None,
    };
    Some((number(x)?, number(y)?))
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    block_coverage::enable();

    // The program is always derived: sources -> compile -> replay the recipe.
    // Measuring the freshly compiled program instead - which this did until it
    // was noticed - reports the K of a program nobody runs, and cannot show any
    // rewrite doing its job. It is still worth having as `--original`, since
    // that is the number the recipe is trying to beat.
    let mut program = celeste_rust::rewrite::program::Program::compile_from_disk()?;
    if cli.original {
        // Measure the plain program as it is actually executed (with the
        // native builtins pinned).
        program.pin_native_builtins()?;
    } else {
        let recipe = celeste_rust::rewrite::recipe::Recipe::load(&cli.recipe)?;
        for entry in &recipe.entries {
            celeste_rust::rewrite::recipe::apply_entry(&mut program, entry)?;
        }
    }
    let fixed_env = program.fixed_env();
    // `Program`'s frame chunk already ends with `__reset_button_states()`, so
    // unlike the old hand-built version there is no separate reset run.
    let frame_cfg = celeste_rust::interpreter::fixed_env::PreparedCfg::new(
        program.frame_cfg().clone(),
    );

    // The init run is not part of a frame; flush it so it doesn't pollute
    // the per-frame maxima. `concrete::initial_state` also swaps the Lua
    // tile_flag_at for the native collision-cache one, exactly like the real
    // runner - without that we would measure ~1,900 instructions of Lua that
    // never actually run.
    let base_state = celeste_rust::concrete::initial_state(&program, &fixed_env)
        .expect("init");
    block_coverage::end_frame_dropping(true);
    assert!(
        current_room(&base_state).is_some(),
        "cannot read the room from the state, so room-load frames cannot be \
         excluded and K would be measured over two different specializations"
    );

    let mut rng = Rng(cli.seed.max(1));
    let mut completed = 0usize;
    let mut aborted = 0usize;

    for seq in 0..cli.sequences {
        let mut state = base_state.clone();
        // A mix of "hold a direction for a while" and pure noise, so we get both
        // sustained high-speed movement (which drives the move_x/move_y unroll
        // bounds) and unusual button combinations (which drive code coverage).
        let mut held: u8 = 0;
        let mut hold_left = 0u32;

        let mut ok = true;
        for _frame in 1..=cli.frames {
            if hold_left == 0 {
                held = (rng.below(64)) as u8;
                hold_left = 1 + rng.below(if seq % 2 == 0 { 8 } else { 2 }) as u32;
            }
            hold_left -= 1;
            let input_byte = held;

            let room_before = current_room(&state);
            state = match celeste_rust::concrete::step_frame(
                &frame_cfg, state, &fixed_env, input_byte,
            ) {
                Ok(s) => s,
                Err(e) => {
                    eprintln!("sequence {} aborted: {}", seq, e);
                    ok = false;
                    break;
                }
            };

            block_coverage::end_frame_dropping(current_room(&state) != room_before);
        }
        if ok {
            completed += 1;
        } else {
            aborted += 1;
        }
    }

    let report = block_coverage::report().expect("coverage enabled");

    println!();
    println!("sequences: {} completed, {} aborted", completed, aborted);
    println!("frames per sequence: {}", cli.frames);
    println!();
    println!(
        "frames measured: {} ({} dropped as room-load frames)",
        report.frames, report.excluded_frames
    );
    println!(
        "instructions one lane executes per frame today: mean {:.0}, max {}",
        report.mean_dynamic, report.max_dynamic
    );
    println!("distinct (function, block) pairs reached: {}", report.distinct_blocks);
    println!();
    println!("K, two bounds:");
    println!(
        "  inlined, loops kept as loops:      {:>7} instructions",
        report.k_static
    );
    println!(
        "  inlined AND fully unrolled:        {:>7} instructions",
        report.k_instructions
    );
    println!(
        "  (only lane-varying control flow actually has to be flattened,",
    );
    println!(
        "   so the real number sits between these)",
    );
    if report.mean_dynamic > 0.0 {
        println!(
            "  => branch-free costs {:.1}x the per-lane work of one path today",
            report.k_instructions as f64 / report.mean_dynamic
        );
    }
    println!();
    println!("blocks needing unrolling (instructions x max executions in one frame):");
    for (func, block, instrs, execs) in report.hot_unrolled.iter().take(20) {
        println!(
            "  {:>7}  = {:>4} x {:>3}   {}::{}",
            instrs * execs,
            instrs,
            execs,
            func,
            block
        );
    }
    println!();
    println!("contribution to K by instruction kind:");
    for (kind, contribution) in report.by_kind.iter() {
        println!(
            "  {:>7}  {:>5.1}%  {}",
            contribution,
            100.0 * *contribution as f64 / report.k_instructions as f64,
            kind
        );
    }
    println!();
    println!("contribution to K by function:");
    for (func, contribution) in report.by_function.iter().take(20) {
        println!(
            "  {:>7}  {:>5.1}%  {}",
            contribution,
            100.0 * *contribution as f64 / report.k_instructions as f64,
            func
        );
    }

    if cli.blocks {
        println!();
        println!("blocks reached (max executions in one frame):");
        for (func, block, max_exec) in &report.reached {
            println!("  {:>4}  {}::{}", max_exec, func, block);
        }
    }

    Ok(())
}
