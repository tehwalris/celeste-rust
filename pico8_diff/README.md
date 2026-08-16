# pico8_diff - differential tests against a real PICO-8

Every case in `cases/` is one Lua file that runs UNCHANGED on both sides:

- ours, via `target/release/lua_run`, which compiles the case with the same
  `frontend::compile`, the same Lua-level builtins
  (`lua/builtin_level_3.lua`, `lua/builtin_level_4.lua`) and the same Rust
  builtins as the game runner;
- a real PICO-8, via a generated `.p8` cart run headless with `pico8 -x`.

The two print streams are diffed. That is the whole harness.

## Running it

```bash
cargo build --release          # run.sh builds lua_run itself if it is missing
pico8_diff/run.sh              # all cases
pico8_diff/run.sh num_sin      # one or more named cases
PICO8=~/bin/pico8 pico8_diff/run.sh    # if pico8 is not on PATH
KEEP=1 pico8_diff/run.sh       # keep the work directory even on success
```

Exit status is 0 only if every case either passed or is a listed known
failure. If `pico8` cannot be found the script says exactly what is missing
and exits 2 - it never skips silently, because a skipped differential test
looks identical to a passing one.

`lua_run` must run from the repo root (the game builtins load `cart/`);
`run.sh` cds there for you.

## Why this exists: `foreach`

Our `foreach` was wrong for months. It was a plain index walk
(`for i=1,#t do ... end` with a length re-check), which SKIPS an element when
the callback deletes the element it was just handed - everything after shifts
down one and the index has already moved past it.

The first fix was to snapshot the table and walk the copy. That came from
lore rather than from evidence, and it was also wrong: it killed the player
at frame 93 of the room (2, 0) community TAS.

The right model is PICO-8's own: `foreach(t, f)` is
`for item in all(t) do f(item) end`, and `all` is deletion-safe by
REMEMBERING THE LAST ITEM IT RETURNED - it only advances its index when that
slot still holds that item. That is what `lua/builtin_level_3.lua` implements
today.

Until now the only evidence for it was that three witness TASes replay
frame-exactly. `cases/seq_foreach.lua` replaces that with a direct
measurement: six scenarios chosen so that the index walk and the snapshot
each fail a *different* pair of them, so no smaller set would distinguish the
three models. It passes.

## The output-format problem

Numbers are never printed with `__print`. Our `format_scalar_number` prints
`whole.raw_fraction` (0.5 comes out as "0.32768") and PICO-8's `tostr` prints
a decimal, so every fraction would be a false diff.

Instead, cases print numbers with `__hex`, which is the exact 16.16 bits
formatted like PICO-8's `tostr(x, true)`: `0x0000.5555`. On the PICO-8 side
that IS `tostr(x, true)` (see `wrap.sh`); on ours it is a small builtin in
`src/bin/lua_run.rs` that formats `Pico8Num::to_bits`.

Hex was chosen over decomposing each number into integers with `flr`, `*`
and `-` inside the case. The decomposition is exact - `i = flr(x)`,
`hi = flr((x - i) * 256)`, `lo = flr(((x - i) * 256 - hi) * 256)` recovers
the whole part and the two fraction bytes with no intermediate leaving the
16.16 range, and it is exact for negative `x` too because `flr` floors -
but it would run the value under test through the very arithmetic under
test. A bug in `flr` or `*` could then hide a bug in `/`, or manufacture a
diff that is not there. Printing raw bits touches no arithmetic at all, is
one line, and is strictly finer-grained.

`__print` is still used for strings, booleans and integer-valued numbers,
where the two formats provably agree. `cases/fmt_basics.lua` is what makes
that "provably" a measurement rather than an assumption; it is the case to
look at first if everything suddenly goes red.

Nothing prints a value that could be nil: `tostr(nil)` is `[nil]` on PICO-8
and `nil` for us. Nil-ness is printed as a boolean instead.

## Known failures

`known_fail.txt` lists the cases that differ today, one per line with the
reason. Those are reported as XFAIL and do not turn the suite red. A case on
that list which starts passing is reported as XPASS and DOES turn it red, so
the list cannot go stale without someone noticing.

None of the listed bugs have been fixed here. Every one of them changes
numbers the search already depends on, so they need to be fixed deliberately,
with the checkpoint hashes re-derived.

## Writing a case

- Only use builtins our interpreter actually has. The Rust ones are
  registered in `create_fixed_env_with_game_builtins` (`src/game_runner.rs`)
  and the Lua ones are in `lua/builtin_level_3.lua` and
  `lua/builtin_level_4.lua`. In particular there is **no `sqrt`, no `cos`,
  no `atan2`, no `sgn`, no `mid`, no `band`/`bor`/`shl`** - a case using one
  of those fails for the wrong reason and teaches nothing. (Real PICO-8 has
  them; `sqrt(2)` is `0x0001.6a09` and `sqrt(-1)` is 0. Cases go here the day
  the builtins do.)
- Build arrays with `add`, not `{"a", "b"}`: our frontend only compiles table
  constructors with named fields.
- Use globals rather than locals captured by a closure.
- Do not write the literal `-32768` unless the case is about literal parsing;
  it does not survive our parser. Use `-32767 - 1`.
- A case must be concrete straight-line code. `lua_run` fails loudly if
  interpretation ends in anything other than exactly one state.
- Keep one divergence per file. Several cases here were split precisely
  because one known-bad line was hiding a dozen good ones behind it.
