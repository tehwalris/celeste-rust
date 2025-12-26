// TODO don't allow later
#![allow(dead_code)]
#![allow(unused_variables)]

use anyhow::Result;
use cart_data::CartData;

// TODO remove unused dependencies
#[macro_use(anyhow)]
extern crate anyhow;
extern crate bv;
extern crate hex;
extern crate regex;
extern crate rustc_hash;
extern crate work_queue;

mod block_flow;
mod cart_data;
mod fixed_point;
mod frontend;
mod input;
mod instruction_flow;
mod interpreter;
mod ir;
mod liveness;
mod pico8_num;
mod tas;

fn main() -> Result<()> {
    let current_dir = std::env::current_dir()?;

    let cart_data = CartData::load(current_dir.join("cart"))?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::interpreter::{
        fixed_env::FixedEnv,
        state::State,
        value::{HeapValue, Value},
    };

    /// Builtin __print: collects the printed value into the state's prints list
    fn builtin_print(mut state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        let printed = if args.is_empty() {
            "".to_string()
        } else {
            match &args[0] {
                Value::String(s) => s.clone(),
                Value::Number(crate::interpreter::value::MaybeVector::Scalar(n)) => {
                    let whole = n.whole_part_as_i16();
                    let frac = n.fraction_part_as_u16();
                    if frac == 0 {
                        format!("{}", whole)
                    } else {
                        format!("{}.{}", whole, frac)
                    }
                }
                Value::Bool(crate::interpreter::value::MaybeVector::Scalar(b)) => {
                    if *b {
                        "true".to_string()
                    } else {
                        "false".to_string()
                    }
                }
                Value::Nil(_) => "nil".to_string(),
                other => format!("{:?}", other),
            }
        };
        state.prints.push(printed);
        Ok(vec![(state, Value::Nil(None))])
    }

    fn create_fixed_env_with_builtins() -> FixedEnv {
        let mut fixed_env = FixedEnv::new();
        fixed_env.add_builtin("__print", builtin_print);
        fixed_env.add_builtin("print", builtin_print);
        fixed_env
    }

    fn create_initial_state_with_builtins(fixed_env: &FixedEnv) -> State {
        let mut state = State::new();

        // Add builtin functions to global_env
        // In the OCaml code, builtins are stored in the heap and their names are in the global scope
        for name in fixed_env.builtin_funs.keys() {
            let heap_id = state.heap.alloc();
            state
                .heap
                .set(heap_id, HeapValue::BuiltinFun(name.clone()));
            state.global_env.insert(name.clone(), heap_id);
        }

        state
    }

    #[test]
    fn test_parse_hello_world() {
        let code = r#"__print("walrus")"#;
        let ast = full_moon::parse(code).expect("Failed to parse");

        // Just test that parsing works
        assert!(ast.nodes().stmts().count() > 0 || ast.nodes().last_stmt().is_some());
    }

    #[test]
    fn test_compile_hello_world() {
        let code = r#"__print("walrus")"#;
        let ast = full_moon::parse(code).expect("Failed to parse");

        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        // The code has no function definitions
        assert!(fun_defs.is_empty());

        // The CFG should have instructions
        assert!(!cfg.entry.instructions.is_empty());
    }

    #[test]
    fn test_builtin_setup() {
        let fixed_env = create_fixed_env_with_builtins();
        let state = create_initial_state_with_builtins(&fixed_env);

        // Check that __print is in the global_env
        assert!(state.global_env.contains_key("__print"));

        // Get the heap_id for __print
        let heap_id = state.global_env.get("__print").unwrap();

        // Check that it's a BuiltinFun
        match state.heap.get(*heap_id) {
            HeapValue::BuiltinFun(name) => assert_eq!(name, "__print"),
            _ => panic!("Expected BuiltinFun"),
        }
    }

    #[test]
    fn test_interpret_hello_world() {
        use crate::interpreter::glue::interpret_cfg;

        let code = r#"__print("walrus")"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, _fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let fixed_env = create_fixed_env_with_builtins();
        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Should have exactly one resulting state
        assert_eq!(result_states.len(), 1);

        // Check that "walrus" was printed
        assert_eq!(result_states[0].0.prints, vec!["walrus"]);
    }

    #[test]
    fn test_interpret_simple_function() {
        use crate::interpreter::glue::interpret_cfg;

        let code = r#"
function greet(name)
    __print(name)
end

greet("bob")
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        // Should have one function definition
        assert_eq!(fun_defs.len(), 1);

        let mut fixed_env = create_fixed_env_with_builtins();
        // Add the function definitions to the fixed_env
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        // Should have exactly one resulting state
        assert_eq!(result_states.len(), 1);

        // Check that "bob" was printed
        assert_eq!(result_states[0].0.prints, vec!["bob"]);
    }

    #[test]
    fn test_interpret_function_with_return() {
        use crate::interpreter::glue::interpret_cfg;

        let code = r#"
function add_one(x)
    return x + 1
end

__print(add_one(5))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(result_states[0].0.prints, vec!["6"]);
    }

    #[test]
    fn test_interpret_if_scopes() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/if_scopes.lua
        let code = r#"
y = nil

function f(x)
  if x then
    local y = 5
  else
    y = 7
  end
end

f(true)
__print(y)
f(false)
__print(y)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // First call f(true) - local y shadows global, so global y stays nil
        // Second call f(false) - global y becomes 7
        assert_eq!(result_states[0].0.prints, vec!["nil", "7"]);
    }

    #[test]
    fn test_interpret_scopes() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/scopes.lua
        let code = r#"
x = 0
y = 7

function g()
  y = 6

  local y = 4

  local f = function(x)
    __print(x)
    x = 2
    __print(x)
    __print(y)
    y = 3
  end

  local h = function()
    local x = 5
    f(x)
    __print(y)
    __print(x)
  end

  y = 5

  return h
end

__print(y)
f = g()
f()
__print(y)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Expected: 7, 5, 2, 5, 3, 5, 6
        // Line by line:
        // __print(y) -> 7 (global y)
        // g() sets global y = 6, creates local y = 4, then sets local y = 5
        // f = g() returns h
        // f() calls h which:
        //   - creates local x = 5
        //   - calls f(5) which:
        //     - __print(x) -> 5 (argument x)
        //     - x = 2 (modifies local x)
        //     - __print(x) -> 2
        //     - __print(y) -> 5 (captured from g, local y was 5)
        //     - y = 3 (modifies captured y)
        //   - __print(y) -> 3 (captured y, now 3)
        //   - __print(x) -> 5 (h's local x, unchanged)
        // __print(y) -> 6 (global y, was set by g())
        assert_eq!(
            result_states[0].0.prints,
            vec!["7", "5", "2", "5", "3", "5", "6"]
        );
    }

    #[test]
    fn test_interpret_call_order() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/call_order.lua
        let code = r#"
function fa(v)
    __print('a')
    __print(v)
    return 'a'
end

function fb(v)
    __print('b')
    __print(v)
    return 'b'
end

function fc()
    fd = fa
    __print('c')
    return 'c'
end

function fe(v1, v2)
    __print('e')
    __print(v1)
    __print(v2)
end

-- fd(fc()) -- attempt to call global 'fd' (a nil value)

fe(fc(), fd('x'))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Call order:
        // 1. fc() is called first, prints "c", sets fd = fa, returns "c"
        // 2. fd('x') is called (which is fa('x')), prints "a", "x", returns "a"
        // 3. fe("c", "a") is called, prints "e", "c", "a"
        assert_eq!(
            result_states[0].0.prints,
            vec!["c", "a", "x", "e", "c", "a"]
        );
    }

    #[test]
    fn test_interpret_call_with_different_number_of_args() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/call_with_different_number_of_args.lua
        let code = r#"
function f(a, b)
  __print(a)
  __print(b)
end

function g(v)
  __print(v)
  return v
end

f(g(1))
f(g(2), g(3))
f(g(4), g(5), g(6))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // f(g(1)): prints 1, then f(1, nil) prints 1, nil
        // f(g(2), g(3)): prints 2, 3, then f(2, 3) prints 2, 3
        // f(g(4), g(5), g(6)): prints 4, 5, 6, then f(4, 5) prints 4, 5 (6 is extra arg, ignored)
        assert_eq!(
            result_states[0].0.prints,
            vec!["1", "1", "nil", "2", "3", "2", "3", "4", "5", "6", "4", "5"]
        );
    }

    #[test]
    fn test_interpret_every_kind_of_if_else() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/every_kind_of_if_else.lua
        let code = r#"
x = true

if x then
  y = 1
  __print(y)
end

if x then
  y = 2
  __print(y)
else
  y = 3
  __print(y)
end

if x then
  y = 4
  __print(y)
elseif x then
  y = 5
  __print(y)
elseif x then
  y = 6
  __print(y)
end

if x then
  y = 7
  __print(y)
elseif x then
  y = 8
  __print(y)
elseif x then
  y = 9
  __print(y)
else
  y = 10
  __print(y)
end

__print(y)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // x is true, so we take the first branch in each if-chain
        // Prints: 1, 2, 4, 7, 7 (final y is 7)
        assert_eq!(
            result_states[0].0.prints,
            vec!["1", "2", "4", "7", "7"]
        );
    }

    #[test]
    fn test_interpret_short_circuit_operators() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/short_circuit_operators.lua
        let code = r#"
function f(s, v)
  __print(s)
  return v
end

__print(f("a", true) and f("b", true) and f("c", true))
__print(f("a", false) and f("b", true) and f("c", true))
__print(f("a", true) and f("b", false) and f("c", true))
__print(f("a", true) and f("b", true) and f("c", false))
__print(f("a", false) and f("b", false) and f("c", false))

__print(f("a", true) or f("b", true) or f("c", true))
__print(f("a", false) or f("b", true) or f("c", true))
__print(f("a", true) or f("b", false) or f("c", true))
__print(f("a", true) or f("b", true) or f("c", false))
__print(f("a", false) or f("b", false) or f("c", false))
__print(f("a", false) or f("b", false) or f("c", true))
__print(f("a", false) or f("b", true) or f("c", false))
__print(f("a", true) or f("b", false) or f("c", false))
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Short-circuit 'and': returns first falsy value or last truthy
        // Short-circuit 'or': returns first truthy value or last falsy
        //
        // f("a", true) and f("b", true) and f("c", true) -> prints a, b, c; result is true
        // f("a", false) and f("b", true) and f("c", true) -> prints a; result is false (short-circuit)
        // f("a", true) and f("b", false) and f("c", true) -> prints a, b; result is false (short-circuit)
        // f("a", true) and f("b", true) and f("c", false) -> prints a, b, c; result is false
        // f("a", false) and f("b", false) and f("c", false) -> prints a; result is false (short-circuit)
        // f("a", true) or f("b", true) or f("c", true) -> prints a; result is true (short-circuit)
        // f("a", false) or f("b", true) or f("c", true) -> prints a, b; result is true (short-circuit)
        // f("a", true) or f("b", false) or f("c", true) -> prints a; result is true (short-circuit)
        // f("a", true) or f("b", true) or f("c", false) -> prints a; result is true (short-circuit)
        // f("a", false) or f("b", false) or f("c", false) -> prints a, b, c; result is false
        // f("a", false) or f("b", false) or f("c", true) -> prints a, b, c; result is true
        // f("a", false) or f("b", true) or f("c", false) -> prints a, b; result is true (short-circuit)
        // f("a", true) or f("b", false) or f("c", false) -> prints a; result is true (short-circuit)
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "a", "b", "c", "true",   // and chain 1
                "a", "false",            // and chain 2
                "a", "b", "false",       // and chain 3
                "a", "b", "c", "false",  // and chain 4
                "a", "false",            // and chain 5
                "a", "true",             // or chain 1
                "a", "b", "true",        // or chain 2
                "a", "true",             // or chain 3
                "a", "true",             // or chain 4
                "a", "b", "c", "false",  // or chain 5
                "a", "b", "c", "true",   // or chain 6
                "a", "b", "true",        // or chain 7
                "a", "true",             // or chain 8
            ]
        );
    }

    #[test]
    fn test_interpret_string_length() {
        use crate::interpreter::glue::interpret_cfg;

        // Basic string length test (subset of lua_tests/string_length.lua without tables)
        let code = r#"
__print(#"")
__print(#" ")
__print(#"walrus")
s = "walrus"
__print(#s)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // #"" = 0
        // #" " = 1
        // #"walrus" = 6
        // #s (s = "walrus") = 6
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "1", "6", "6"]
        );
    }

    #[test]
    fn test_interpret_normal_operators() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/normal_operators.lua (lines 1-22, without table operations)
        let code = r#"
__print(1 + 1)
__print(5 - 7)
__print(7 - 5)
__print(3 * 4)
__print(12 / 3)
__print(2 - 2 / 4 + 0.5)
__print(12 % 5)
-- __print(12.5 % 5 + 0.5)  -- Skip: fractional modulo not yet implemented in Pico8Num
__print(-7)
__print(-(-7))
__print(1 < 2)
__print(1 <= 2)
__print(1 > 2)
__print(1 >= 2)
__print(1 == 2)
__print(1 ~= 2)
__print(not (1 == 2))
__print(not true)
__print("wal".."rus")
__print("12"..3)
__print((1).."23")
__print(((1).."23") == "123")

function_a = function () end
function_b = function () end
__print(function_a == function_a)
__print(function_a ~= function_a)
__print(function_a == function_b)
__print(function_a ~= function_b)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "2",        // 1 + 1
                "-2",       // 5 - 7
                "2",        // 7 - 5
                "12",       // 3 * 4
                "4",        // 12 / 3
                "2",        // 2 - 2/4 + 0.5 = 2 - 0.5 + 0.5 = 2
                "2",        // 12 % 5
                // (skipped fractional modulo)
                "-7",       // -7
                "7",        // -(-7)
                "true",     // 1 < 2
                "true",     // 1 <= 2
                "false",    // 1 > 2
                "false",    // 1 >= 2
                "false",    // 1 == 2
                "true",     // 1 ~= 2
                "true",     // not (1 == 2)
                "false",    // not true
                "walrus",   // "wal".."rus"
                "123",      // "12"..3
                "123",      // (1).."23"
                "true",     // ((1).."23") == "123"
                "true",     // function_a == function_a
                "false",    // function_a ~= function_a
                "false",    // function_a == function_b (different closures)
                "true",     // function_a ~= function_b
            ]
        );
    }

    #[test]
    fn test_interpret_for_range() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/for_range.lua (first part)
        let code = r#"
for i=0,5 do
    __print(i)
end
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Should print 0, 1, 2, 3, 4, 5
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "1", "2", "3", "4", "5"]
        );
    }

    #[test]
    fn test_interpret_for_break() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/for_break.lua
        let code = r#"
for i=0,5 do
    if i == 3 then
        break
    end
    __print(i)
end
__print("end")
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        // Should print 0, 1, 2, then break, then "end"
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "1", "2", "end"]
        );
    }

    #[test]
    fn test_interpret_simple_table() {
        use crate::interpreter::glue::interpret_cfg;

        // Simple nested table test
        let code = r#"
a = { b = 10 }
__print(a.b)
a.b = 20
__print(a.b)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["10", "20"]
        );
    }

    #[test]
    fn test_interpret_properties() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/properties.lua
        let code = r#"
a = { b = { c = 0, d = nil } }
__print(a.b.c)
a.b.c = 123
__print(a.b.c)
__print(a.b.d)
a.b.d = 456
__print(a.b.d)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["0", "123", "nil", "456"]
        );
    }

    #[test]
    fn test_interpret_tables_object() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/tables.lua (first two parts, without array tables)
        let code = r#"
x = {}
x.a = 1
x.b = 7
x.c = 'hello'
__print(x.a)
__print(x.b)
__print(x.c)
x.c = 'world'
__print(x.c)

y = {a = 1, b = 7, c = 'hello'}
__print(y.a)
__print(y.b)
__print(y.c)
y.c = 'world'
__print(y.c)
"#;
        let ast = full_moon::parse(code).expect("Failed to parse");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile");

        let mut fixed_env = create_fixed_env_with_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec!["1", "7", "hello", "world", "1", "7", "hello", "world"]
        );
    }
}
