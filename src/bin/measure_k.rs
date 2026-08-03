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
use celeste_rust::game_runner::{
    create_initial_state_with_builtins, inject_tile_flag_at_builtin,
};
use celeste_rust::interpreter::glue::interpret_cfg;
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

fn set_concrete_buttons(state: &mut State, input_byte: u8) {
    let buttons = [
        (input_byte & 1) != 0,
        (input_byte & 2) != 0,
        (input_byte & 4) != 0,
        (input_byte & 8) != 0,
        (input_byte & 16) != 0,
        (input_byte & 32) != 0,
    ];
    let button_states_global_id = state
        .global_env
        .get("__button_states")
        .expect("__button_states not found");
    let button_states_id = match state.heap.get(*button_states_global_id) {
        HeapValue::Value(Value::Pointer(id)) => *id,
        HeapValue::ArrayTable(_) => *button_states_global_id,
        other => panic!("__button_states unexpected: {:?}", other),
    };
    let array_items = match state.heap.get(button_states_id) {
        HeapValue::ArrayTable(items) => items.clone(),
        other => panic!("__button_states not an array: {:?}", other),
    };
    for (i, &pressed) in buttons.iter().enumerate() {
        let item_ptr_id = array_items[i];
        let value_id = match state.heap.get(item_ptr_id) {
            HeapValue::Value(Value::Pointer(id)) => *id,
            _ => item_ptr_id,
        };
        state
            .heap
            .set(value_id, HeapValue::Value(Value::Bool(MaybeVector::Scalar(pressed))));
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
    if !cli.original {
        let recipe = celeste_rust::rewrite::recipe::Recipe::load(&cli.recipe)?;
        for entry in &recipe.entries {
            celeste_rust::rewrite::recipe::apply_entry(&mut program, entry)?;
        }
    }
    let fixed_env = program.fixed_env();
    let initial_state = create_initial_state_with_builtins(&fixed_env);
    // `Program`'s frame chunk already ends with `__reset_button_states()`, so
    // unlike the hand-built version there is no separate reset run.
    let frame_cfg = program.frame_cfg().clone();
    let cfg = program.init_cfg().clone();

    // The init run is not part of a frame; flush it so it doesn't pollute
    // the per-frame maxima.
    let init_result = interpret_cfg(cfg, initial_state, &fixed_env).expect("init");
    assert_eq!(init_result.len(), 1);
    let mut base_state = init_result.into_iter().next().unwrap().0;
    // The real runner swaps the Lua tile_flag_at for the native collision-cache
    // one right after init. Without this we would measure ~1,900 instructions of
    // Lua that never actually run.
    inject_tile_flag_at_builtin(&mut base_state);
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
            set_concrete_buttons(&mut state, input_byte);
            let result = match interpret_cfg(frame_cfg.clone(), state, &fixed_env) {
                Ok(r) => r,
                Err(e) => {
                    eprintln!("sequence {} aborted: {}", seq, e);
                    ok = false;
                    break;
                }
            };
            if result.len() != 1 {
                eprintln!("sequence {} branched into {} states", seq, result.len());
                ok = false;
                break;
            }
            state = result.into_iter().next().unwrap().0;

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
