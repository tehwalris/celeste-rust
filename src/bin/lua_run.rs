//! Run one standalone Lua file through our interpreter and dump its prints.
//!
//! This exists for `pico8_diff/`: the same .lua source is run here and on a
//! real PICO-8, and the two print streams are diffed. It is deliberately a
//! thin wrapper - it must run the SAME frontend, the same Lua-level builtins
//! (`lua/builtin_level_3.lua`, `lua/builtin_level_4.lua`) and the same Rust
//! builtins as the game, or a passing diff would prove nothing about the
//! game.
//!
//! Must be run from the repo root: the game builtins load `cart/`.

use anyhow::{anyhow, bail, Context, Result};
use celeste_rust::game_runner::{
    create_fixed_env_with_game_builtins, create_initial_state_with_builtins,
};
use celeste_rust::interpreter::glue::interpret_cfg;
use celeste_rust::interpreter::state::State;
use celeste_rust::interpreter::value::{MaybeVector, Value};
use celeste_rust::{frontend, pico8_num::Pico8Num};
use clap::Parser;

#[derive(Parser)]
#[command(name = "lua_run")]
#[command(about = "Interpret a standalone .lua file and print its __print output")]
struct Cli {
    /// Path to the .lua file to run
    file: String,
}

/// `__hex(x)`: the exact 16.16 bits, formatted like PICO-8's `tostr(x, true)`.
///
/// The harness prints every number through this rather than through
/// `__print`. Two reasons, both learned the hard way while designing the
/// cases: `__print`'s number format (`format_scalar_number`) is
/// `whole.raw_fraction`, which agrees with `tostr` on nothing fractional; and
/// the alternative - decomposing a number into integers with `flr`, `*` and
/// `-` inside the case - would run the value under test through the very
/// arithmetic under test, so a bug in `flr` or `*` could hide a bug in `/`.
/// Printing raw bits touches no arithmetic at all.
fn builtin_hex(mut state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    let n: Pico8Num = match args.as_slice() {
        [Value::Number(MaybeVector::Scalar(n))] => *n,
        [other] => return Err(anyhow!("__hex: expected a scalar number, got {:?}", other)),
        _ => return Err(anyhow!("__hex requires exactly 1 argument")),
    };
    let bits = n.to_bits();
    state
        .prints
        .push(format!("0x{:04x}.{:04x}", bits >> 16, bits & 0xffff));
    Ok(vec![(state, Value::Nil(None))])
}

fn main() -> Result<()> {
    let cli = Cli::parse();

    let case = std::fs::read_to_string(&cli.file)
        .with_context(|| format!("reading case {}", cli.file))?;
    let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")
        .context("reading lua/builtin_level_3.lua (run from the repo root)")?;
    let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")
        .context("reading lua/builtin_level_4.lua (run from the repo root)")?;
    let full = format!("{}\n{}\n{}\n", level_3, level_4, case);
    // The case is compiled as one chunk with the builtin prelude in front of
    // it, so full_moon's line numbers are offset. Say by how much rather than
    // leaving the case author to count.
    let prelude_lines = full.lines().count() - case.lines().count();

    let ast = full_moon::parse(&full).map_err(|e| {
        anyhow!(
            "parse {}: {:?} (line numbers include {} lines of builtin prelude)",
            cli.file,
            e,
            prelude_lines
        )
    })?;
    let (cfg, fun_defs) =
        frontend::compile(&ast).with_context(|| format!("compiling {}", cli.file))?;

    let mut fixed_env = create_fixed_env_with_game_builtins();
    fixed_env.add_builtin("__hex", builtin_hex);
    for fun_def in fun_defs {
        fixed_env.add_fun_def(fun_def);
    }
    let initial_state = create_initial_state_with_builtins(&fixed_env);

    let result_states = interpret_cfg(cfg, initial_state, &fixed_env)
        .with_context(|| format!("interpreting {}", cli.file))?;

    // A case is concrete straight-line code, so anything other than one
    // outcome means the case reached the abstract machinery (an unknown
    // boolean, a merge) and its print stream is not a single sequence to
    // diff against PICO-8. Loud, because a "0 states" result would otherwise
    // silently look like a case that printed nothing.
    if result_states.len() != 1 {
        bail!(
            "{}: expected exactly 1 resulting state, got {} - the case must be \
             concrete straight-line code",
            cli.file,
            result_states.len()
        );
    }

    let mut out = String::new();
    for line in &result_states[0].0.prints {
        out.push_str(line);
        out.push('\n');
    }
    print!("{}", out);
    Ok(())
}
