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
}
