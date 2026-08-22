# Divergence audit: the tracer vs the IR pipeline vs real PICO-8

2026-08-22. Run by an Opus subagent at Philippe's suggestion, as "an extra
sanity check". Recorded VERBATIM below, because the useful thing on a
re-audit is not the list of bugs - those get fixed - but the record of
what was LOOKED AT and found clean, which is the part a summary destroys.

The two implementations compared:

* **(A)** the tracer, `src/trace/` - walks the Lua AST with `full_moon`,
  heap concrete, values symbolic.
* **(B)** the IR pipeline, `crates/celeste-ir/src/frontend.rs` (Lua -> IR)
  plus `crates/celeste-interp/src/interpreter/` - the reference the
  project has trusted for years.

Settled against real PICO-8 (`~/pico-8/pico8 -x`) wherever a question
could be.

## Status as of this writing

| # | finding | resolution | commit |
|---|---|---|---|
| 1 | `break` a no-op in `run_for_symbolic` | fixed in (A), MATCH | b510784 |
| 2 | no per-block scope in (A) | fixed in (A), MATCH | b510784 |
| 3 | `tile_flag_at` non-zero flag answers false in BOTH | (A) RAISES unless provably absent; (B) untouched | ec2c5fd |
| 4 | `==` on two booleans is node identity in (A) | fixed in (A), MATCH | dec1684 |
| 5 | `#` is not `luaH_getn` | (A) RAISES where capacity matters | dec1684 |
| 6 | constructs one side answers and the other refuses | `min(5)` panic fixed (RAISE); rest untouched | dec1684 |
| 7 | assignment evaluates LHS/RHS in opposite orders | fixed in (A), MATCH | dec1684 |
| 8 | (B): `local x = <expr mentioning x>` sees the new nil cell | OPEN, unreachable, in (B) | - |
| 9 | `__array_table_drop_last` representation differs | no fix needed | - |
| 10 | `run_for_symbolic` obligation over-strict | fixed as part of #1 | b510784 |

Note one staleness in the report: it flags `lua/probe/tables.lua` cases
10-11 as using multiple returns. That was true while it was running and
had already been fixed when it finished.

---

## The report, verbatim

I read `CLAUDE.md`, `plans/tracing.md` T1-T7, both implementations end to
end, and settled every question I could against real PICO-8
(`~/pico-8/pico8 -x`, scratch carts under `/tmp/audit`). No repo file was
modified.

### 1. `break` is a no-op inside a symbolically-bounded numeric `for` - (A) only. REACHABLE.

- **(A)** `src/trace/interp.rs:591` - `for_body` rewrites `Flow::Break` to `Flow::Normal` and returns it; `run_for_symbolic` (`interp.rs:537`, `:541`) puts that outcome straight back into `running`, so the state re-enters the body on the next `k`. The concrete-bound sibling `run_for` (`interp.rs:598`) does it correctly (moves `Break` to `done`) and its own comment records that this exact bug was fixed there once. The `Break->Normal` map at `interp.rs:571` is dead code - a symptom.
- **(B)** `crates/celeste-ir/src/frontend.rs:1400` - `break` compiles to `UnconditionalBranch { target: break_label }`, which `NumericFor` binds to the loop's `join_label` (`frontend.rs:1273`). The loop is left.
- **PICO-8**: `local n=0 for i=1,8 do n=n+1 break end printh(n)` -> `1`; `local amount=5 local x=0 for i=0,abs(amount) do if i>=2 then break end x=x+1 end printh(x)` -> `2`.
- **Reach**: `unroll_bound` (`interp.rs`, key `"abs(amount)"`) sends exactly `obj.move_x` (`lua/celeste-minimal.lua:659-672`) and `obj.move_y` (`:675-688`) down this path, and both `break` on hitting a solid. It is currently **masked**: the else arm (`spd.x=0`, `rem.x=0`) is idempotent and `is_solid` at the unchanged position stays true, so the extra iterations re-derive the same answer. The "not solid" arm is still explored under an unsatisfiable guard (`g AND c AND NOT c`; `Graph::fold`, `src/transpile/graph.rs:348`, folds only constant `And`), so it costs nodes and states and is selected away rather than being right by construction.
- **Resolution**: fix (A) - carry `Flow::Break` out of `for_body` and drop those states out of `running`, exactly as `run_for` does.

### 2. (A) has no per-block scope: a `local` in an `if`/`else` body lands in the enclosing **function** scope. REACHABLE (as a merge failure).

- **(A)** `src/trace/interp.rs:135` - `exec_block` never calls `new_scope`; `LocalAssignment` declares into `s.scope` (`interp.rs:226`, `:232`). Only `for_body` (`:583`) and `call_value` create scopes.
- **(B)** `crates/celeste-ir/src/frontend.rs:1037` - `compile_block` clones `locals` on entry, so the binding dies at `end`.
- **PICO-8**: `local m=1 if true then local m=2 printh(m) end printh(m)` -> `2` then `1`; `if true then local leaked=7 end printh(leaked)` -> `[nil]`.
- **Reach**: no name in the cart is shadowed, so no wrong *value*. But leaked names are part of `Shape::scopes` (`src/trace/heap.rs`), so the two arms of `if this.dash_time>0 then ... else <maxrun, accel, deccel, maxfall, gravity, d_full, d_half> end` (`celeste-minimal.lua:125-131`) and of `if this.djump>0 and dash then ... local v_input ...` (`:194`) end up with different scope key sets and **cannot merge**. That is a direct, avoidable source of tracer state fan-out.
- **Resolution**: fix (A) - push a child scope in `exec_block` (or at least around `if`/`elseif`/`else` bodies).

### 3. `tile_flag_at` with a non-zero flag silently answers `false` - BOTH. Latent, high-risk.

- **(A)** `src/trace/interp.rs:1259` (`if f.as_i16() != Some(0) { false }`) and `src/trace/eval.rs:158`.
- **(B)** `crates/celeste-interp/src/game_runner.rs:579` and `:718` (`if flagi == 0 { solid_at } else { false }`, with a "not ideal but allows testing" comment).
- **Real cart**: `cart/flag-data.txt` has flag 4 set on 16 tiles (66-69, 82-85, 98-101, 114-117); by `cart/map-data.txt` they occur in room (3,1) and every room of map rows 2 and 3.
- **Reach**: `ice_at`/`is_ice` runs every frame (`celeste-minimal.lua:100`, `:161`) and gates `accel=0.05` and the wall-slide `maxfall=0.4`. Rooms (0,0)/(1,0)/(2,0) have no ice, so today the answer is accidentally right; the first search in a row-1+ room gets silently wrong physics in both engines simultaneously, so the differential gate would not catch it.
- **Resolution**: add a runtime guard in both - refuse `tile_flag_at` with `flag != 0` unless the loaded room provably contains no tile with that flag.

### 4. (A): `==` / `~=` between two booleans is NodeId identity, i.e. a constant. Unreachable in the cart.

- **(A)** `src/trace/interp.rs:921` - `_ if op == Cmp::Eq => { let same = a == b; ... }`, and `Value::Bool(a) == Value::Bool(b)` in `heap.rs`'s `PartialEq` compares `D::Bool`, which for `Symbolic` is a `NodeId`. Two distinct symbolic booleans compare `false` unconditionally; `b == true` is `false` unconditionally.
- **(B)** `crates/celeste-interp/src/interpreter/op.rs:446-455` - real per-lane compare for `Bool==Bool`, and `UnknownBool` (i.e. branch both ways) for `Bool==UnknownBool` and `UnknownBool==UnknownBool`.
- **PICO-8**: `local a=true local b=false printh(a==b) printh(a==true) printh(a~=b)` -> `false`, `true`, `true`.
- **Reach**: none - every `==`/`~=` in the three Lua files takes numbers, table pointers or nil. (`del`'s `v == target` and `foreach`'s `tbl[i] == prev` are pointer/nil only.)
- **Resolution**: fix (A) - give `(Bool, Bool)` a domain-level equality (`not(xor)`), or bail loudly. Note (A) already bails loudly for closure-vs-closure at `interp.rs:909`; booleans need the same treatment.

### 5. (A)'s `#` is not `luaH_getn`: it models the array part's used length, not its capacity. Unreachable.

Real Lua sizes the array part in powers of two at rehash, so the border depends on capacity.

- `t={} t[1]=1 t[2]=2 t[3]=3 t[2]=nil printh(#t)` -> PICO-8 **1**. (A) -> 3: `arr=[1,Nil,3]`, last slot non-nil so `len()` takes the `else` branch (`src/trace/heap.rs:198`) and returns `arr.len()`. (B) -> 3.
- `t={} t[1]=1 t[2]=2 t[4]=4 printh(#t)` -> PICO-8 **4**. (A) -> **2**: `4` goes to `ints` (`heap.rs:220-243`) and the `ints` walk (`heap.rs:200-207`) stops at the missing 3. (B) -> 4 (dense gap fill). **A and B disagree.**
- Same result via `add`: `local h={} add(h,1) add(h,2) add(h,3) h[2]=nil printh(#h)` -> PICO-8 `1`, (A) `3`.
- **Reach**: none. Nothing in the cart punches an interior hole (`del` shifts left then drops the last), and the one sparse write, `got_fruit[1+level_index()]` (`celeste-minimal.lua:592`, `:419`), never has `#` taken of it.
- **Resolution**: extend `lua/probe/tables.lua` with append-built-then-punched and sparse-after-append cases, and either model array capacity or guard `#` (refuse when `arr` holds an interior `Nil` or `ints` is non-empty).

### 6. Constructs one side answers and the other refuses. All unreachable in the cart.

| construct | (A) | (B) | PICO-8 |
|---|---|---|---|
| `not nil` / `not 0` / `not {}` | correct (`interp.rs:658` truthiness) | **errors** - `interpret_not` (`op.rs:12`) accepts only `Bool`/`UnknownBool` | `true`, `false`, `false` |
| `#"hello"` | bails, "`#` of a non-table" (`interp.rs:713`) | correct (`op.rs:28`) | `5` |
| `t[0]`, `t[-1]` | correct via `ints` | errors, "Index is less than 1" (`core_interpreter.rs:424`) | works; `t[0]="zero"` reads back, `#t`==1 |
| `t[0.5]` | bails, "fractional table index" (`interp.rs:976`) | errors (`core_interpreter.rs:422`) | works; `u[0.5]="half"` reads back, `#u`==0 |
| `{1,2,3}` (positional) | supported | **`unimplemented!()` panic** (`frontend.rs:785`) | works |
| `..` | bails | correct (`op.rs:470`) | `"x".."y"` == `xy` |
| `min(5)` / `max(5)` | **Rust index panic** on `args[1]` (`interp.rs:1327`; `abs`/`flr`/`sin` index `args[0]` unchecked at `:1197`) | clean error | `0` and `5` |
| multiple return / multiple assignment | refuses (`interp.rs:196`, `:248`) | refuses (`frontend.rs:1063`, `:1374`) | works |

Heads-up: `lua/probe/tables.lua` has just grown cases 10-11 using `local inc, get = counter()` / `return inc, get`; **both** implementations will refuse those, so that probe cannot pass as written.

### 7. Assignment evaluates LHS and RHS in opposite orders. Unreachable.

- **(A)** `src/trace/interp.rs:241-260` evaluates the RHS first, then walks the LHS suffixes.
- **(B)** `crates/celeste-ir/src/frontend.rs:1058-1078` emits the LHS (`compile_var`) stream first.
- **PICO-8**: `function f() printh("lhs-index") return 1 end function g() printh("rhs") return 9 end local t={} t[f()]=g()` -> prints `lhs-index` then `rhs`. (B) matches.
- **Reach**: only `init_object(platform,tx*8,ty*8).dir=-1` (`celeste-minimal.lua:741`, `:743`) has a side-effecting LHS, and its RHS is a literal.
- **Resolution**: fix (A).

### 8. (B): `local x = <expr mentioning x>` sees the new, nil cell. Unreachable.

- **(B)** `crates/celeste-ir/src/frontend.rs:1104` inserts `name -> lhs_id` into `locals` *before* `compile_rhs_expression` on the next line.
- **(A)** `interp.rs:225-227` evaluates then declares - correct.
- **PICO-8**: `local z=7 local z=z printh(z)` -> `7`.
- **Reach**: no `local N = ...N...` in the three Lua files (checked all 36 `local` statements).
- **Resolution**: fix (B). Related and also unreachable: (A) declares multi-name `local a,b = b,a` name-by-name (`interp.rs:219-238`) rather than evaluating all RHS first - (B) refuses multi-name locals outright.

### 9. `__array_table_drop_last` differs in representation. Reachable, observationally equal.

- **(A)** `src/trace/interp.rs:1318` - `tab.set_index(n, Nil)` where `n = len()`, leaving the array slot with an explicit `Nil`. Faithful: Lua never shrinks the array part.
- **(B)** `game_runner.rs` `builtin_array_table_drop_last` - `items.pop()`.
- I hand-checked del-middle / del-first / del-last / del-only / del-empty and the subsequent `add`: reads and `#` agree in both. But (A)'s trailing `Nil`s are part of `Shape::tables`, so a heap that has had an object deleted has a different shape from an otherwise-equal heap that has not, and the two will not merge. Worth knowing when hunting fan-out; no fix needed.

### 10. `run_for_symbolic` applies the "loop finished" obligation to states that already returned or broke.

`src/trace/interp.rs:566` ANDs `finished` into `s.ok` for every surviving state, including ones that left the loop early and therefore never needed the bound. Sound (over-strict => deopt, never a wrong answer), but it costs declines. Precision only.

## Categories checked, nothing found

- **Arithmetic / comparison / fixed point.** Single-sourced: (A) routes through `domain::Concrete` -> `Pico8Num`, (B) through `op.rs` -> the same `Pico8Num`. Re-measured against the console: `-1%8==7`, `7%-3==1`, `-7%3==2`, `5%0==0`, `1/0==32768` (`0x7fff.ffff`), `-1/0==-32768`, `flr(-1.5)==-2`, `flr(-7/2)==-4`, `-0==0`, `1/3==0.3333`. All match `crates/celeste-core/src/pico8_num.rs` (saturating `abs` vs wrapping `neg`, saturating division, `rem_euclid`, tabulated `sin`).
- **Truthiness.** Only `nil` and `false` are falsy in both (`interp.rs:658`; `flow.rs:352-388`). `0` and `""` are truthy in both and in PICO-8 (verified).
- **`and`/`or` in value position.** Both return the operand, not a boolean, and short-circuit ((B) puts the RHS stream inside the `continue` block, `frontend.rs:539-601`). Verified `1 and 2`->2, `nil and 2`->nil, `false or "hi"`->hi, `0 or 9`->0, `nil or false`->false.
- **Numeric `for`.** Limit evaluated once, loop variable immune to body assignment, fresh per iteration for closures - same in both and in PICO-8 (`for i=1,lim do lim=1` -> 1,2,3; `for i=1,3 do i=i+10` -> 11,12,13; loop-captured closures -> 1,2,3).
- **Table key normalisation.** `t[1]` == `t[1.0]`, `t["1"]` distinct - same in both and in PICO-8.
- **Equality across types.** `1=="1"`, `nil==false`, `1==true` all `false` in PICO-8 and in both.
- **Method sugar `a:b()`, `while`, `repeat`, generic `for`, `goto`, varargs, metatables.** None appear in `lua/celeste-minimal.lua` or the two builtin files; both implementations refuse them loudly.
- **Arguments.** Both pad missing with nil and drop extras.
- **Undefined global / absent field.** nil in both and in PICO-8.
- **Strings.** Neither interns; both compare by value; both usable as table keys; both take full_moon's raw literal text, so escape sequences are undecoded in both (agreeing with each other, differing from PICO-8 - no escapes in the cart).
- **Builtins vs `builtin_level_3/4.lua`.** `add`/`del`/`foreach`/`count`/`btn` come from Lua in both (the Lua `function add` overwrites (B)'s native `add` at the same heap cell). `min`/`max`/`abs`/`flr`/`sin`/`mget`/`fget` are native in both with matching semantics. One shared gap: `rnd` is used by `balloon.init` (`:343`) and `chest.update` (`:520`) and is defined nowhere - both fail identically on a room with a balloon or a chest.
