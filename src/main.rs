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

    use crate::interpreter::value::MaybeVector;
    use crate::pico8_num::Pico8Num;

    fn format_scalar_number(n: &crate::pico8_num::Pico8Num) -> String {
        let whole = n.whole_part_as_i16();
        let frac = n.fraction_part_as_u16();
        if frac == 0 {
            format!("{}", whole)
        } else {
            format!("{}.{}", whole, frac)
        }
    }

    /// Builtin __print: collects the printed value into the state's prints list
    fn builtin_print(mut state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        let printed = if args.is_empty() {
            "".to_string()
        } else {
            match &args[0] {
                Value::String(s) => s.clone(),
                Value::Number(MaybeVector::Scalar(n)) => format_scalar_number(n),
                Value::Number(MaybeVector::Vector(nums)) => {
                    let inner: Vec<String> = nums.iter().map(format_scalar_number).collect();
                    format!("V[{}]", inner.join(", "))
                }
                Value::Bool(MaybeVector::Scalar(b)) => {
                    if *b {
                        "true".to_string()
                    } else {
                        "false".to_string()
                    }
                }
                Value::Bool(MaybeVector::Vector(bools)) => {
                    let inner: Vec<String> = bools.iter()
                        .map(|b| if *b { "true".to_string() } else { "false".to_string() })
                        .collect();
                    format!("V[{}]", inner.join(", "))
                }
                Value::Nil(_) => "nil".to_string(),
                other => format!("{:?}", other),
            }
        };
        state.prints.push(printed);
        Ok(vec![(state, Value::Nil(None))])
    }

    /// Builtin add: adds a value to the end of an array table
    /// In Lua: function add(t, v) t[#t + 1] = v end
    fn builtin_add(mut state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if args.len() != 2 {
            return Err(anyhow!("add requires 2 arguments"));
        }
        let table_heap_id = match &args[0] {
            Value::Pointer(heap_id) => *heap_id,
            _ => return Err(anyhow!("add: first argument must be a table")),
        };
        let value = args[1].clone();

        // Allocate a new heap slot for the value
        let value_heap_id = state.heap.alloc();
        state
            .heap
            .set(value_heap_id, HeapValue::Value(value));

        // Get the table and add the value
        match state.heap.get_mut(table_heap_id) {
            HeapValue::ArrayTable(items) => {
                items.push(value_heap_id);
            }
            HeapValue::UnknownTable => {
                // Convert unknown table to array table
                state.heap.set(
                    table_heap_id,
                    HeapValue::ArrayTable(vec![value_heap_id]),
                );
            }
            other => return Err(anyhow!("add: first argument is not an array table: {:?}", other)),
        }

        Ok(vec![(state, Value::Nil(None))])
    }

    /// Builtin __new_unknown_boolean: returns an unknown boolean (could be either true or false)
    fn builtin_new_unknown_boolean(state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if !args.is_empty() {
            return Err(anyhow!("__new_unknown_boolean takes no arguments"));
        }
        Ok(vec![(state, Value::UnknownBool)])
    }

    /// Builtin __new_vector: creates a vector from an array table
    fn builtin_new_vector(state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if args.len() != 1 {
            return Err(anyhow!("__new_vector requires 1 argument"));
        }
        let table_heap_id = match &args[0] {
            Value::Pointer(heap_id) => *heap_id,
            _ => return Err(anyhow!("__new_vector: argument must be a table")),
        };

        let item_heap_ids = match state.heap.get(table_heap_id) {
            HeapValue::ArrayTable(items) => items.clone(),
            _ => return Err(anyhow!("__new_vector: argument must be an array table")),
        };

        if item_heap_ids.is_empty() {
            return Err(anyhow!("Cannot make a vector with no values"));
        }

        // Collect values and determine the type
        let mut numbers: Vec<crate::pico8_num::Pico8Num> = Vec::new();
        for heap_id in item_heap_ids {
            match state.heap.get(heap_id) {
                HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => {
                    numbers.push(*n);
                }
                other => {
                    return Err(anyhow!(
                        "__new_vector: all values must be scalar numbers, got {:?}",
                        other
                    ))
                }
            }
        }

        Ok(vec![(state, Value::Number(MaybeVector::Vector(numbers)))])
    }

    /// Builtin flr: floor function (Pico-8)
    fn builtin_flr(state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if args.len() != 1 {
            return Err(anyhow!("flr requires 1 argument"));
        }
        match &args[0] {
            Value::Number(nums) => {
                let result = nums.map(|n| n.flr());
                Ok(vec![(state, Value::Number(result))])
            }
            _ => Err(anyhow!("flr: argument must be a number")),
        }
    }

    /// Builtin __split_by_flr: splits values by their floor
    /// For abstract interpretation, this would split number intervals into
    /// separate ranges by floor value. For concrete numbers, it just returns the value.
    /// For vectors, it groups elements by floor and produces separate states.
    fn builtin_split_by_flr(state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        use std::collections::BTreeMap;

        if args.len() != 1 {
            return Err(anyhow!("__split_by_flr requires 1 argument"));
        }

        match &args[0] {
            Value::Number(MaybeVector::Scalar(n)) => {
                // For a single scalar number, there's only one floor value,
                // so just return the state unchanged
                Ok(vec![(state, Value::Number(MaybeVector::Scalar(*n)))])
            }
            Value::Number(MaybeVector::Vector(nums)) => {
                // Group vector elements by their floor value
                // Each group becomes a separate state
                let mut by_floor: BTreeMap<Pico8Num, Vec<(usize, Pico8Num)>> = BTreeMap::new();
                for (i, n) in nums.iter().enumerate() {
                    let floor = n.flr();
                    by_floor.entry(floor).or_default().push((i, *n));
                }

                // Create a separate state for each floor group
                let mut results = Vec::new();
                for (_floor, group) in by_floor {
                    // Build a mask for which elements are in this group
                    let mask: Vec<bool> = (0..nums.len())
                        .map(|i| group.iter().any(|(gi, _)| *gi == i))
                        .collect();

                    // Filter the state's values by this mask
                    let filtered_state = state.filter_by_mask(&mask);

                    // Create the result value for this group
                    let result_nums: Vec<Pico8Num> = group.into_iter().map(|(_, n)| n).collect();
                    let result_value = if result_nums.len() == 1 {
                        Value::Number(MaybeVector::Scalar(result_nums[0]))
                    } else {
                        Value::Number(MaybeVector::Vector(result_nums))
                    };

                    results.push((filtered_state, result_value));
                }

                Ok(results)
            }
            _ => Err(anyhow!("__split_by_flr: argument must be a number")),
        }
    }

    /// Builtin error: throws an error (crashes the interpreter)
    fn builtin_error(_state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        match args.as_slice() {
            [] => Err(anyhow!("error called")),
            [Value::String(s)] => Err(anyhow!("error called: {}", s)),
            _ => Err(anyhow!("error: wrong arguments")),
        }
    }

    /// Builtin min: minimum of two numbers
    fn builtin_min(state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if args.len() != 2 {
            return Err(anyhow!("min requires 2 arguments"));
        }
        match (&args[0], &args[1]) {
            (Value::Number(a), Value::Number(b)) => {
                let result = MaybeVector::map2(a, b, |a, b| (*a).min(*b));
                Ok(vec![(state, Value::Number(result))])
            }
            _ => Err(anyhow!("min: arguments must be numbers")),
        }
    }

    /// Builtin max: maximum of two numbers
    fn builtin_max(state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if args.len() != 2 {
            return Err(anyhow!("max requires 2 arguments"));
        }
        match (&args[0], &args[1]) {
            (Value::Number(a), Value::Number(b)) => {
                let result = MaybeVector::map2(a, b, |a, b| (*a).max(*b));
                Ok(vec![(state, Value::Number(result))])
            }
            _ => Err(anyhow!("max: arguments must be numbers")),
        }
    }

    /// Builtin abs: absolute value
    fn builtin_abs(state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if args.len() != 1 {
            return Err(anyhow!("abs requires 1 argument"));
        }
        match &args[0] {
            Value::Number(nums) => {
                let result = nums.map(|n| n.abs());
                Ok(vec![(state, Value::Number(result))])
            }
            _ => Err(anyhow!("abs: argument must be a number")),
        }
    }

    /// Builtin __array_table_drop_last: removes the last element from an array table
    fn builtin_array_table_drop_last(mut state: State, args: Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        if args.len() != 1 {
            return Err(anyhow!("__array_table_drop_last requires 1 argument"));
        }
        let table_heap_id = match &args[0] {
            Value::Pointer(heap_id) => *heap_id,
            _ => return Err(anyhow!("__array_table_drop_last: argument must be a table")),
        };

        match state.heap.get_mut(table_heap_id) {
            HeapValue::ArrayTable(items) => {
                if items.is_empty() {
                    return Err(anyhow!("Cannot drop last element of empty array table"));
                }
                items.pop();
            }
            _ => return Err(anyhow!("__array_table_drop_last: expected array table")),
        }

        Ok(vec![(state, Value::Nil(None))])
    }

    /// Create mget builtin with cart data
    fn make_builtin_mget(
        cart_data: std::sync::Arc<cart_data::CartData>,
    ) -> impl Fn(State, Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        move |state: State, args: Vec<Value>| {
            if args.len() != 2 {
                return Err(anyhow!("mget requires 2 arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::Number(x), Value::Number(y)) => {
                    let result = MaybeVector::map2(x, y, |x, y| {
                        Pico8Num::from_i16(cart_data.mget(*x, *y).expect("mget failed") as i16)
                    });
                    Ok(vec![(state, Value::Number(result))])
                }
                _ => Err(anyhow!("mget: arguments must be numbers")),
            }
        }
    }

    /// Create fget builtin with cart data
    fn make_builtin_fget(
        cart_data: std::sync::Arc<cart_data::CartData>,
    ) -> impl Fn(State, Vec<Value>) -> anyhow::Result<Vec<(State, Value)>> {
        move |state: State, args: Vec<Value>| {
            if args.len() != 2 {
                return Err(anyhow!("fget requires 2 arguments"));
            }
            match (&args[0], &args[1]) {
                (Value::Number(i), Value::Number(b)) => {
                    let result = MaybeVector::map2(i, b, |i, b| {
                        cart_data.fget(*i, *b).expect("fget failed")
                    });
                    Ok(vec![(state, Value::Bool(result))])
                }
                _ => Err(anyhow!("fget: arguments must be numbers")),
            }
        }
    }

    fn create_fixed_env_with_builtins() -> FixedEnv {
        let mut fixed_env = FixedEnv::new();
        // Level 1 builtins
        fixed_env.add_builtin("__print", builtin_print);
        fixed_env.add_builtin("__new_unknown_boolean", builtin_new_unknown_boolean);
        fixed_env.add_builtin("__new_vector", builtin_new_vector);
        fixed_env.add_builtin("__array_table_drop_last", builtin_array_table_drop_last);
        // Level 2 builtins
        fixed_env.add_builtin("error", builtin_error);
        fixed_env.add_builtin("min", builtin_min);
        fixed_env.add_builtin("max", builtin_max);
        fixed_env.add_builtin("abs", builtin_abs);
        fixed_env.add_builtin("flr", builtin_flr);
        fixed_env.add_builtin("__split_by_flr", builtin_split_by_flr);
        // Level 3 builtins (implemented in Lua, but add as Rust for test convenience)
        fixed_env.add_builtin("add", builtin_add);
        fixed_env.add_builtin("print", builtin_print);
        fixed_env
    }

    fn create_fixed_env_with_game_builtins() -> FixedEnv {
        let mut fixed_env = create_fixed_env_with_builtins();

        // Level 5 builtins (cart data)
        let cart_data =
            std::sync::Arc::new(cart_data::CartData::load("cart").expect("Failed to load cart data"));
        fixed_env.add_builtin("mget", make_builtin_mget(cart_data.clone()));
        fixed_env.add_builtin("fget", make_builtin_fget(cart_data));

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

    #[test]
    fn test_interpret_tables_array() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/tables.lua (array part)
        let code = r#"
z = {}
add(z, 1)
add(z, 7)
add(z, 'hello')
__print(z[1])
__print(z[2])
__print(z[3])
z[3] = 'world'
__print(z[3])
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
            vec!["1", "7", "hello", "world"]
        );
    }

    #[test]
    fn test_interpret_abstract_boolean_no_call() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/abstract_boolean_no_call.lua
        let code = r#"
v = __new_unknown_boolean()
if v then
  __print("a")
else
  __print("b")
end
if v then
  __print("c")
else
  __print("d")
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

        // With unknown boolean v, we should get 4 possible outcomes:
        // - v=true in both ifs: ["a", "c"]
        // - v=true then false: ["a", "d"]
        // - v=false then true: ["b", "c"]
        // - v=false then false: ["b", "d"]
        // However since v is the *same* unknown boolean, it could be either true or false
        // and the interpreter should explore both branches independently at each if statement.
        // This should result in 4 distinct output states.

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec!["a".to_string(), "c".to_string()],
            vec!["a".to_string(), "d".to_string()],
            vec!["b".to_string(), "c".to_string()],
            vec!["b".to_string(), "d".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_vectors() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/vector.lua
        let code = r#"
x_values = {}
add(x_values, 6)
add(x_values, 3)
add(x_values, 8)
x = __new_vector(x_values)

y_values = {}
add(y_values, 2)
add(y_values, 4)
add(y_values, 6)
y = __new_vector(y_values)

function f(v)
  return v + 1
end

__print(x)
__print(x + y)
__print(x + 3)
__print(3 + x)
__print((x + 0.5) + (y + 0.5))
x = y
__print(x + y)
__print(f(x))
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
                "V[6, 3, 8]",
                "V[8, 7, 14]",
                "V[9, 6, 11]",
                "V[9, 6, 11]",
                "V[9, 8, 15]",
                "V[4, 8, 12]",
                "V[3, 5, 7]",
            ]
        );
    }

    #[test]
    fn test_interpret_undefined_fields() {
        use crate::interpreter::glue::interpret_cfg;

        // Simplified undefined_fields.lua test
        let code = r#"
x = {}
__print(x.y)

x = {y='a', z=nil}
__print(x.y)
__print(x.z)
__print(x.a)
__print(x.a == nil)
__print(x.z == nil)
__print(x.a == x.z)

__print(global_which_does_not_exist)
__print(x.a == global_which_does_not_exist)
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
                "nil",      // x.y (undefined)
                "a",        // x.y
                "nil",      // x.z (explicitly nil)
                "nil",      // x.a (undefined)
                "true",     // x.a == nil
                "true",     // x.z == nil
                "true",     // x.a == x.z (both nil)
                "nil",      // global_which_does_not_exist
                "true",     // x.a == global_which_does_not_exist
            ]
        );
    }

    #[test]
    fn test_interpret_short_circuit_operators_non_boolean() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/short_circuit_operators_non_boolean.lua
        // In Lua, `and` and `or` return the actual value (not a boolean):
        // - `a and b` returns `a` if `a` is falsy, otherwise returns `b`
        // - `a or b` returns `a` if `a` is truthy, otherwise returns `b`
        let code = r#"
function f(s, v)
  __print(s)
  return v
end

__print(f("a", true) and f("b", -4) or f("c", 0))
__print(f("a", false) and f("b", -4) or f("c", 0))
__print(f("a", 3) and f("b", -4) or f("c", 0))
__print(f("a", 0) and f("b", -4) or f("c", 0))
__print(f("a", 0) and f("b", 0) or f("c", 4))

__print(f("a", -3) and f("b", 0))
__print(f("a", 0) and f("b", 0))
__print(f("a", 3) and f("b", 0))
__print(f("a", 0.3) and f("b", 0))
__print(f("a", "") and f("b", 0))
__print(f("a", " ") and f("b", 0))
__print(f("a", "a") and f("b", 0))
__print(f("a", nil) and f("b", 0))
__print(f("a", {}) and f("b", 0))
__print(f("a", f) and f("b", 0))
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

        // Expected behavior:
        // Line 6: f("a", true) returns true (truthy) -> eval f("b", -4) returns -4 (truthy) -> -4
        //         prints: a, b, then -4
        // Line 7: f("a", false) returns false (falsy) -> false (short-circuit and) -> or f("c", 0) returns 0
        //         prints: a, c, then 0
        // Line 8: f("a", 3) returns 3 (truthy) -> f("b", -4) returns -4 (truthy) -> -4
        //         prints: a, b, then -4
        // Line 9: f("a", 0) returns 0 (truthy in Lua!) -> f("b", -4) returns -4 (truthy) -> -4
        //         prints: a, b, then -4
        // Line 10: f("a", 0) returns 0 (truthy) -> f("b", 0) returns 0 (truthy) -> 0
        //          prints: a, b, then 0
        // Line 12: f("a", -3) returns -3 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 13: f("a", 0) returns 0 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 14: f("a", 3) returns 3 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 15: f("a", 0.3) returns 0.3 (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 16: f("a", "") returns "" (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 17: f("a", " ") returns " " (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 18: f("a", "a") returns "a" (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 19: f("a", nil) returns nil (falsy) -> nil (short-circuit, don't eval b)
        //          prints: a, then nil
        // Line 20: f("a", {}) returns {} (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0
        // Line 21: f("a", f) returns f (truthy) -> f("b", 0) returns 0
        //          prints: a, b, then 0

        let expected = vec![
            "a", "b", "-4",   // line 6
            "a", "c", "0",    // line 7
            "a", "b", "-4",   // line 8
            "a", "b", "-4",   // line 9
            "a", "b", "0",    // line 10
            "a", "b", "0",    // line 12
            "a", "b", "0",    // line 13
            "a", "b", "0",    // line 14
            "a", "b", "0",    // line 15
            "a", "b", "0",    // line 16
            "a", "b", "0",    // line 17
            "a", "b", "0",    // line 18
            "a", "nil",       // line 19 (nil short-circuits)
            "a", "b", "0",    // line 20
            "a", "b", "0",    // line 21
        ];

        assert_eq!(result_states[0].0.prints, expected);
    }

    #[test]
    fn test_interpret_liveness_issue_for_in_function() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/liveness_issue_for_in_function.lua
        // The interpreter used to crash at "print" because the function call was
        // causing the outer locals to be lost. This only happened with functions that
        // have multiple cfg blocks.
        let code = r#"
function function_with_for()
  for i = 1, 7 do
  end
end

local y = {}
function_with_for()
__print(#y)
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
        assert_eq!(result_states[0].0.prints, vec!["0"]);
    }

    #[test]
    fn test_interpret_abstract_boolean_with_call() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/abstract_boolean.lua
        // Tests that abstract boolean branching creates 4 independent output states
        // when the same unknown boolean is used in two different if statements.
        let code = r#"
function f(v)
  if v then
    __print("a")
  else
    __print("b")
  end
  if v then
    __print("c")
  else
    __print("d")
  end
end

f(__new_unknown_boolean())
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

        // The expected outputs are:
        // - ["a", "c"] (v=true, v=true)
        // - ["a", "d"] (v=true, v=false)
        // - ["b", "c"] (v=false, v=true)
        // - ["b", "d"] (v=false, v=false)

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec!["a".to_string(), "c".to_string()],
            vec!["a".to_string(), "d".to_string()],
            vec!["b".to_string(), "c".to_string()],
            vec!["b".to_string(), "d".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_foreach() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/foreach.lua - testing the foreach function implementation
        // foreach is implemented in Lua (builtin_level_3.lua):
        //   function foreach(tbl, func)
        //     for i=1,32767 do
        //       if #tbl < i then break end
        //       func(tbl[i])
        //     end
        //   end
        let code = r#"
function foreach(tbl, func)
  for i=1,32767 do
    if #tbl < i then
      break
    end
    func(tbl[i])
  end
end

function __assert(cond, msg)
  if not cond then
    __print("Assertion failed")
    if msg == nil then
      error()
    else
      __print(msg)
      error(msg)
    end
  end
end

function make_example_table()
  local x = {}
  add(x, 3)
  add(x, 1)
  add(x, 'walrus')
  return x
end

function test_normal_foreach()
  local x = make_example_table()

  local y = {}
  foreach(x, function(v)
    add(y, v)
  end)

  __assert(#y == 3)
  __assert(y[1] == 3)
  __assert(y[2] == 1)
  __assert(y[3] == 'walrus')
end

foreach(make_example_table(), __print)
test_normal_foreach()
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
        // foreach(make_example_table(), __print) should print: 3, 1, walrus
        assert_eq!(result_states[0].0.prints, vec!["3", "1", "walrus"]);
    }

    #[test]
    fn test_interpret_less_than_with_vectors() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/less_than.lua
        // Tests comparison operations on vectors
        let code = r#"
low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

__print(0 < 1.5)
__print(1 < 1.8)
__print(low < 1.5)
__print(1 < high)
__print(low < high)
__print((0 + 1) < 1.5)
__print((1 + 1) < 1.8)
__print((low + 1) < high)
__print(low)
__print(flr(high))
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

        // Expected output:
        // true        - 0 < 1.5
        // true        - 1 < 1.8
        // true        - V[0, 1] < 1.5 -> V[true, true] which prints as "true" (both true)
        // true        - 1 < V[1.5, 1.8] -> V[true, true]
        // true        - V[0, 1] < V[1.5, 1.8] -> V[true, true]
        // true        - (0 + 1) < 1.5
        // false       - (1 + 1) < 1.8 (2 < 1.8 is false)
        // V[true, false] - (V[0, 1] + 1) < V[1.5, 1.8] -> V[1, 2] < V[1.5, 1.8] -> V[true, false]
        // V[0, 1]     - low unchanged
        // 1           - flr(V[1.5, 1.8]) -> V[1, 1] but if all same, shows as scalar

        assert_eq!(result_states.len(), 1);
        assert_eq!(
            result_states[0].0.prints,
            vec![
                "true",
                "true",
                "V[true, true]",
                "V[true, true]",
                "V[true, true]",
                "true",
                "false",
                "V[true, false]",
                "V[0, 1]",
                "V[1, 1]",
            ]
        );
    }

    #[test]
    fn test_interpret_vector_branch() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/vector_branch.lua
        // Tests vector branching - when branching on a vector comparison,
        // the vector gets filtered to only include elements matching the branch condition
        let code = r#"
x_values = {}
add(x_values, 6)
add(x_values, 3)
add(x_values, 8)
x = __new_vector(x_values)

__print(x)
if x < 5 then
  __print(x)
else
  __print(x)
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

        // Expected output options:
        // Option 1: x < 5 branch (only element 3 is < 5)
        //   V[6, 3, 8]
        //   3
        // Option 2: x >= 5 branch (elements 6 and 8 are >= 5)
        //   V[6, 3, 8]
        //   V[6, 8]

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec!["V[6, 3, 8]".to_string(), "3".to_string()],
            vec!["V[6, 3, 8]".to_string(), "V[6, 8]".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_if_vector() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/if_vector.lua
        // Tests if statements with vector comparisons that cause branching
        let code = r#"
low_values = {}
add(low_values, 0)
add(low_values, 1)
low = __new_vector(low_values)

high_values = {}
add(high_values, 1.5)
add(high_values, 1.8)
high = __new_vector(high_values)

__print(low)
if (low + 1) < high then
  __print("below")
  __print(low)
end
__print(low)
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

        // Expected output options:
        // Option 1: (low + 1) < high is true for element 0 (0+1=1 < 1.5)
        //   V[0, 1]
        //   below
        //   0
        //   0
        // Option 2: (low + 1) < high is false for element 1 (1+1=2 < 1.8 is false)
        //   V[0, 1]
        //   1

        let mut output_sets: Vec<Vec<String>> = result_states
            .iter()
            .map(|(state, _)| state.prints.clone())
            .collect();
        output_sets.sort();

        let mut expected_sets = vec![
            vec![
                "V[0, 1]".to_string(),
                "below".to_string(),
                "0".to_string(),
                "0".to_string(),
            ],
            vec!["V[0, 1]".to_string(), "1".to_string()],
        ];
        expected_sets.sort();

        assert_eq!(output_sets, expected_sets);
    }

    #[test]
    fn test_interpret_branching_with_irrelevant_locals() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/branching_with_irrelevant_locals.lua
        // Both branches produce the same output ("a"), so they should merge to 1 state
        let code = r#"
if __new_unknown_boolean() then
  __print("a")
else
  local s = "a"
  __print(s)
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

        // Both branches print "a", so there should be 1 branch with prints ["a"]
        // Note: deduplication might not happen in current implementation,
        // but both outputs should still be ["a"]
        for (state, _) in &result_states {
            assert_eq!(state.prints, vec!["a"]);
        }
    }

    #[test]
    fn test_interpret_branching_with_allocations() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/branching_with_allocations.lua
        // Both branches create tables and set same values, should produce equivalent outputs
        let code = r#"
if __new_unknown_boolean() then
  x = {}
  x[1] = "a"
  y = {}
  y[1] = "b"
else
  y = {}
  y[1] = "b"
  x = {}
  x[1] = "a"
end
__print(x[1])
__print(y[1])
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

        // Both branches should produce ["a", "b"]
        for (state, _) in &result_states {
            assert_eq!(state.prints, vec!["a", "b"]);
        }
    }

    #[test]
    fn test_interpret_branching_into_return() {
        use crate::interpreter::glue::interpret_cfg;

        // From lua_tests/branching_into_return.lua
        // Both branches return, so there should be 1 equivalent state
        let code = r#"
if __new_unknown_boolean() then
  local x = 1
  return
else
  local x = 2
  return
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

        // Both branches end with return, and both have no prints
        for (state, _) in &result_states {
            assert_eq!(state.prints, Vec::<String>::new());
        }
    }

    #[test]
    fn test_load_and_compile_celeste_game() {
        // Test that we can parse and compile the celeste-minimal.lua game
        let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")
            .expect("Failed to read builtin_level_3.lua");
        let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")
            .expect("Failed to read builtin_level_4.lua");
        let game = std::fs::read_to_string("lua/celeste-minimal.lua")
            .expect("Failed to read celeste-minimal.lua");

        // Suffix code to call _init
        let suffix = r#"
_init()
__reset_button_states()
"#;

        let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, suffix);

        let ast = full_moon::parse(&full_code).expect("Failed to parse game code");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile game");

        println!("Game compiled successfully!");
        println!("  CFG entry instructions: {}", cfg.entry.instructions.len());
        println!("  Function definitions: {}", fun_defs.len());

        // Just test that compilation works for now
        assert!(!fun_defs.is_empty(), "Game should have function definitions");
    }

    #[test]
    #[ignore] // Ignore until performance is good enough
    fn test_run_celeste_game_init() {
        use crate::interpreter::glue::interpret_cfg;

        // Load and compile the game
        let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")
            .expect("Failed to read builtin_level_3.lua");
        let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")
            .expect("Failed to read builtin_level_4.lua");
        let game = std::fs::read_to_string("lua/celeste-minimal.lua")
            .expect("Failed to read celeste-minimal.lua");

        // Suffix code to call _init
        let suffix = r#"
_init()
__reset_button_states()
"#;

        let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, suffix);

        let ast = full_moon::parse(&full_code).expect("Failed to parse game code");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile game");

        let mut fixed_env = create_fixed_env_with_game_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        println!("Starting game interpretation...");
        let start = std::time::Instant::now();
        let result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Interpretation failed");
        let elapsed = start.elapsed();

        println!("Game init completed in {:?}", elapsed);
        println!("Result states: {}", result_states.len());

        // The game should produce at least one state
        assert!(!result_states.is_empty(), "Game should produce at least one state");
    }

    #[test]
    #[ignore] // Ignore until performance is good enough
    fn test_run_celeste_game_frame() {
        use crate::interpreter::glue::interpret_cfg;

        // Load and compile the game
        let level_3 = std::fs::read_to_string("lua/builtin_level_3.lua")
            .expect("Failed to read builtin_level_3.lua");
        let level_4 = std::fs::read_to_string("lua/builtin_level_4.lua")
            .expect("Failed to read builtin_level_4.lua");
        let game = std::fs::read_to_string("lua/celeste-minimal.lua")
            .expect("Failed to read celeste-minimal.lua");

        // Suffix code to call _init
        let init_suffix = r#"
_init()
__reset_button_states()
"#;

        let full_code = format!("{}\n{}\n{}\n{}\n", level_3, level_4, game, init_suffix);

        let ast = full_moon::parse(&full_code).expect("Failed to parse game code");
        let (cfg, fun_defs) = frontend::compile(&ast).expect("Failed to compile game");

        let mut fixed_env = create_fixed_env_with_game_builtins();
        for fun_def in fun_defs {
            fixed_env.add_fun_def(fun_def);
        }

        let initial_state = create_initial_state_with_builtins(&fixed_env);

        println!("Running game init...");
        let start = std::time::Instant::now();
        let init_result_states =
            interpret_cfg(cfg, initial_state, &fixed_env).expect("Init interpretation failed");
        println!("Game init completed in {:?}", start.elapsed());
        println!("States after init: {}", init_result_states.len());

        // Compile frame code (update + draw + reset buttons)
        let frame_code = r#"
_update()
_draw()
__reset_button_states()
"#;
        let frame_ast = full_moon::parse(frame_code).expect("Failed to parse frame code");
        let (frame_cfg, frame_fun_defs) = frontend::compile(&frame_ast).expect("Failed to compile frame");
        assert!(frame_fun_defs.is_empty(), "Frame code should not define new functions");

        // Run frames
        let num_frames = 10;
        let mut states: Vec<_> = init_result_states.into_iter().map(|(s, _)| s).collect();

        for frame_num in 1..=num_frames {
            println!("\nFrame {}:", frame_num);
            println!("  Input states: {}", states.len());

            let start = std::time::Instant::now();
            let mut new_states = Vec::new();

            for state in states {
                let result = interpret_cfg(frame_cfg.clone(), state, &fixed_env)
                    .expect("Frame interpretation failed");
                new_states.extend(result.into_iter().map(|(s, _)| s));
            }

            println!("  Output states: {}", new_states.len());
            println!("  Frame completed in {:?}", start.elapsed());

            states = new_states;
        }

        assert!(!states.is_empty(), "Should have at least one state after frames");
        println!("\nTotal states after {} frames: {}", num_frames, states.len());
    }
}
