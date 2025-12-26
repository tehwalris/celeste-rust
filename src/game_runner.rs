//! Game runner module - sets up and runs the Celeste game interpreter

use anyhow::Result;
use crate::interpreter::{
    fixed_env::FixedEnv,
    state::State,
    value::{HeapValue, Value, MaybeVector},
};
use crate::pico8_num::Pico8Num;
use crate::cart_data;

fn format_scalar_number(n: &Pico8Num) -> String {
    let whole = n.whole_part_as_i16();
    let frac = n.fraction_part_as_u16();
    if frac == 0 {
        format!("{}", whole)
    } else {
        format!("{}.{}", whole, frac)
    }
}

/// Builtin __print: collects the printed value into the state's prints list
fn builtin_print(mut state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
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
                if *b { "true".to_string() } else { "false".to_string() }
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
fn builtin_add(mut state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if args.len() != 2 {
        return Err(anyhow!("add requires 2 arguments"));
    }
    let table_heap_id = match &args[0] {
        Value::Pointer(heap_id) => *heap_id,
        _ => return Err(anyhow!("add: first argument must be a table")),
    };
    let value = args[1].clone();
    let value_heap_id = state.heap.alloc();
    state.heap.set(value_heap_id, HeapValue::Value(value));

    match state.heap.get_mut(table_heap_id) {
        HeapValue::ArrayTable(items) => { items.push(value_heap_id); }
        HeapValue::UnknownTable => {
            state.heap.set(table_heap_id, HeapValue::ArrayTable(vec![value_heap_id]));
        }
        other => return Err(anyhow!("add: first argument is not an array table: {:?}", other)),
    }
    Ok(vec![(state, Value::Nil(None))])
}

fn builtin_new_unknown_boolean(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if !args.is_empty() {
        return Err(anyhow!("__new_unknown_boolean takes no arguments"));
    }
    Ok(vec![(state, Value::UnknownBool)])
}

fn builtin_new_vector(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
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
    let mut numbers: Vec<Pico8Num> = Vec::new();
    for heap_id in item_heap_ids {
        match state.heap.get(heap_id) {
            HeapValue::Value(Value::Number(MaybeVector::Scalar(n))) => { numbers.push(*n); }
            other => return Err(anyhow!("__new_vector: all values must be scalar numbers, got {:?}", other)),
        }
    }
    Ok(vec![(state, Value::Number(MaybeVector::Vector(numbers)))])
}

fn builtin_flr(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
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

fn builtin_split_by_flr(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    use std::collections::BTreeMap;
    if args.len() != 1 {
        return Err(anyhow!("__split_by_flr requires 1 argument"));
    }
    match &args[0] {
        Value::Number(MaybeVector::Scalar(n)) => {
            Ok(vec![(state, Value::Number(MaybeVector::Scalar(*n)))])
        }
        Value::Number(MaybeVector::Vector(nums)) => {
            let mut by_floor: BTreeMap<Pico8Num, Vec<(usize, Pico8Num)>> = BTreeMap::new();
            for (i, n) in nums.iter().enumerate() {
                let floor = n.flr();
                by_floor.entry(floor).or_default().push((i, *n));
            }
            let mut results = Vec::new();
            for (_floor, group) in by_floor {
                let mask: Vec<bool> = (0..nums.len())
                    .map(|i| group.iter().any(|(gi, _)| *gi == i))
                    .collect();
                let filtered_state = state.filter_by_mask(&mask);
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

fn builtin_error(_state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    match args.as_slice() {
        [] => Err(anyhow!("error called")),
        [Value::String(s)] => Err(anyhow!("error called: {}", s)),
        _ => Err(anyhow!("error: wrong arguments")),
    }
}

fn builtin_min(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if args.len() != 2 { return Err(anyhow!("min requires 2 arguments")); }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => {
            let result = MaybeVector::map2(a, b, |a, b| (*a).min(*b));
            Ok(vec![(state, Value::Number(result))])
        }
        _ => Err(anyhow!("min: arguments must be numbers")),
    }
}

fn builtin_max(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if args.len() != 2 { return Err(anyhow!("max requires 2 arguments")); }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => {
            let result = MaybeVector::map2(a, b, |a, b| (*a).max(*b));
            Ok(vec![(state, Value::Number(result))])
        }
        _ => Err(anyhow!("max: arguments must be numbers")),
    }
}

fn builtin_abs(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if args.len() != 1 { return Err(anyhow!("abs requires 1 argument")); }
    match &args[0] {
        Value::Number(nums) => {
            let result = nums.map(|n| n.abs());
            Ok(vec![(state, Value::Number(result))])
        }
        _ => Err(anyhow!("abs: argument must be a number")),
    }
}

fn builtin_array_table_drop_last(mut state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if args.len() != 1 { return Err(anyhow!("__array_table_drop_last requires 1 argument")); }
    let table_heap_id = match &args[0] {
        Value::Pointer(heap_id) => *heap_id,
        _ => return Err(anyhow!("__array_table_drop_last: argument must be a table")),
    };
    match state.heap.get_mut(table_heap_id) {
        HeapValue::ArrayTable(items) => {
            if items.is_empty() { return Err(anyhow!("Cannot drop last element of empty array table")); }
            items.pop();
        }
        _ => return Err(anyhow!("__array_table_drop_last: expected array table")),
    }
    Ok(vec![(state, Value::Nil(None))])
}

fn make_builtin_mget(
    cart_data: std::sync::Arc<cart_data::CartData>,
) -> impl Fn(State, Vec<Value>) -> Result<Vec<(State, Value)>> {
    move |state: State, args: Vec<Value>| {
        if args.len() != 2 { return Err(anyhow!("mget requires 2 arguments")); }
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

fn make_builtin_fget(
    cart_data: std::sync::Arc<cart_data::CartData>,
) -> impl Fn(State, Vec<Value>) -> Result<Vec<(State, Value)>> {
    move |state: State, args: Vec<Value>| {
        if args.len() != 2 { return Err(anyhow!("fget requires 2 arguments")); }
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

pub fn create_fixed_env_with_builtins() -> FixedEnv {
    let mut fixed_env = FixedEnv::new();
    fixed_env.add_builtin("__print", builtin_print);
    fixed_env.add_builtin("__new_unknown_boolean", builtin_new_unknown_boolean);
    fixed_env.add_builtin("__new_vector", builtin_new_vector);
    fixed_env.add_builtin("__array_table_drop_last", builtin_array_table_drop_last);
    fixed_env.add_builtin("error", builtin_error);
    fixed_env.add_builtin("min", builtin_min);
    fixed_env.add_builtin("max", builtin_max);
    fixed_env.add_builtin("abs", builtin_abs);
    fixed_env.add_builtin("flr", builtin_flr);
    fixed_env.add_builtin("__split_by_flr", builtin_split_by_flr);
    fixed_env.add_builtin("add", builtin_add);
    fixed_env.add_builtin("print", builtin_print);
    fixed_env
}

pub fn create_fixed_env_with_game_builtins() -> FixedEnv {
    let mut fixed_env = create_fixed_env_with_builtins();
    let cart_data = std::sync::Arc::new(
        cart_data::CartData::load("cart").expect("Failed to load cart data")
    );
    fixed_env.add_builtin("mget", make_builtin_mget(cart_data.clone()));
    fixed_env.add_builtin("fget", make_builtin_fget(cart_data));
    fixed_env
}

pub fn create_initial_state_with_builtins(fixed_env: &FixedEnv) -> State {
    let mut state = State::new();
    for name in fixed_env.builtin_funs.keys() {
        let heap_id = state.heap.alloc();
        state.heap.set(heap_id, HeapValue::BuiltinFun(name.clone()));
        state.global_env.insert(name.clone(), heap_id);
    }
    state
}
