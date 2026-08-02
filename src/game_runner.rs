//! Game runner module - sets up and runs the Celeste game interpreter

use anyhow::Result;
use crate::interpreter::{
    fixed_env::FixedEnv,
    state::State,
    value::{HeapValue, Value, MaybeVector},
};
use crate::pico8_num::{Pico8Num, Pico8NumInterval};
use crate::cart_data;
use crate::collision_cache::CollisionCache;

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

    match state.heap.get(table_heap_id) {
        HeapValue::ArrayTable(_) => {
            state.heap.modify(table_heap_id, |v| {
                if let HeapValue::ArrayTable(items) = v {
                    items.push(value_heap_id);
                }
            });
        }
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

fn builtin_new_vector(mut state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
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
    // Update state's vector_size to match the vector length
    state.vector_size = numbers.len();
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
        Value::NumberInterval(MaybeVector::Scalar(interval)) => {
            // For an interval, flr only works if all values have the same floor
            let low_flr = interval.low.flr();
            let high_flr = interval.high.flr();
            if low_flr != high_flr {
                return Err(anyhow!(
                    "flr of interval [{:?}, {:?}] spans multiple floors ({:?} to {:?}). Call __split_by_flr first.",
                    interval.low, interval.high, low_flr, high_flr
                ));
            }
            Ok(vec![(state, Value::Number(MaybeVector::Scalar(low_flr)))])
        }
        Value::NumberInterval(MaybeVector::Vector(intervals)) => {
            // For a vector of intervals, each must have a single floor value
            let mut floors = Vec::with_capacity(intervals.len());
            for interval in intervals {
                let low_flr = interval.low.flr();
                let high_flr = interval.high.flr();
                if low_flr != high_flr {
                    return Err(anyhow!(
                        "flr of interval [{:?}, {:?}] spans multiple floors. Call __split_by_flr first.",
                        interval.low, interval.high
                    ));
                }
                floors.push(low_flr);
            }
            let result = if floors.len() == 1 {
                Value::Number(MaybeVector::Scalar(floors[0]))
            } else {
                Value::Number(MaybeVector::Vector(floors))
            };
            Ok(vec![(state, result)])
        }
        _ => Err(anyhow!("flr: argument must be a number")),
    }
}

/// Split an interval into sub-intervals where all values have the same floor.
/// Returns a sequence of non-overlapping intervals covering the original.
fn split_interval_by_floor(interval: Pico8NumInterval) -> Vec<Pico8NumInterval> {
    let one = Pico8Num::from_i16(1);
    let mut results = Vec::new();
    let mut current = interval.low;

    while current <= interval.high {
        let floor_val = current.flr();
        let next_floor = floor_val + one;
        // largest_with_same_floor is floor_val + 0.ffff = next_floor - epsilon
        let largest_with_same_floor = next_floor.next_smallest();

        let sub_low = current;
        let sub_high = interval.high.min(largest_with_same_floor);

        results.push(Pico8NumInterval::new(sub_low, sub_high));
        current = next_floor;
    }

    results
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
                let filtered_state = state.filter_by_mask_clone(&mask, crate::interpreter::state::FILTER_SPLIT_FLR);
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
        Value::NumberInterval(MaybeVector::Scalar(interval)) => {
            // Split interval into sub-intervals by floor value
            let sub_intervals = split_interval_by_floor(*interval);
            let results: Vec<(State, Value)> = sub_intervals
                .into_iter()
                .map(|sub| (state.clone(), Value::NumberInterval(MaybeVector::Scalar(sub))))
                .collect();
            Ok(results)
        }
        Value::NumberInterval(MaybeVector::Vector(intervals)) => {
            // For a vector of intervals:
            // 1. Take the union of all intervals
            // 2. Split the union into same-floor sub-intervals
            // 3. For each sub-interval, filter to elements that intersect
            if intervals.is_empty() {
                return Ok(vec![(state, Value::NumberInterval(MaybeVector::Vector(vec![])))]);
            }

            // Compute union of all intervals
            let common_interval = intervals.iter().cloned().reduce(|a, b| a.union(&b)).unwrap();

            // Split into same-floor sub-intervals
            let sub_intervals = split_interval_by_floor(common_interval);

            let mut results = Vec::new();
            for sub in sub_intervals {
                // For each vector element, compute intersection with sub
                let intersections: Vec<Option<Pico8NumInterval>> = intervals
                    .iter()
                    .map(|int| int.intersect(&sub))
                    .collect();

                // Create mask: true if this element intersects
                let mask: Vec<bool> = intersections.iter().map(|opt| opt.is_some()).collect();
                let mask_count = mask.iter().filter(|&&b| b).count();

                if mask_count == 0 {
                    continue; // No elements in this floor range
                }

                let filtered_state = state.filter_by_mask_clone(&mask, crate::interpreter::state::FILTER_SPLIT_FLR);

                // Collect the intersected intervals (non-None values)
                let result_intervals: Vec<Pico8NumInterval> = intersections
                    .into_iter()
                    .filter_map(|opt| opt)
                    .collect();

                let result_value = if result_intervals.len() == 1 {
                    Value::NumberInterval(MaybeVector::Scalar(result_intervals[0]))
                } else {
                    Value::NumberInterval(MaybeVector::Vector(result_intervals))
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
    match state.heap.get(table_heap_id) {
        HeapValue::ArrayTable(items) => {
            if items.is_empty() { return Err(anyhow!("Cannot drop last element of empty array table")); }
            state.heap.modify(table_heap_id, |v| {
                if let HeapValue::ArrayTable(items) = v {
                    items.pop();
                }
            });
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

/// Builtin tile_flag_at using precomputed collision cache
/// tile_flag_at(x, y, w, h, flag) -> bool
fn make_builtin_tile_flag_at(
    cart_data: std::sync::Arc<cart_data::CartData>,
    collision_cache: std::sync::Arc<CollisionCache>,
) -> impl Fn(State, Vec<Value>) -> Result<Vec<(State, Value)>> {
    move |state: State, args: Vec<Value>| {
        if args.len() != 5 {
            return Err(anyhow!("tile_flag_at requires 5 arguments (x, y, w, h, flag)"));
        }

        // For now, only support flag=0 (solid) which is what solid_at uses
        let flag = match &args[4] {
            Value::Number(MaybeVector::Scalar(n)) => n.as_i16().ok_or_else(|| anyhow!("flag must be integer"))?,
            _ => return Err(anyhow!("tile_flag_at: flag must be scalar number")),
        };

        if flag != 0 {
            // Fall back to computed version for non-solid flags
            return tile_flag_at_computed(&cart_data, &collision_cache, state, &args);
        }

        // Extract scalar w, h (these are typically constant)
        let (w, h) = match (&args[2], &args[3]) {
            (Value::Number(MaybeVector::Scalar(w)), Value::Number(MaybeVector::Scalar(h))) => {
                (w.as_i16().ok_or_else(|| anyhow!("w must be integer"))?,
                 h.as_i16().ok_or_else(|| anyhow!("h must be integer"))?)
            }
            _ => return tile_flag_at_computed(&cart_data, &collision_cache, state, &args),
        };

        // Handle x, y which may be vectors
        match (&args[0], &args[1]) {
            (Value::Number(x), Value::Number(y)) => {
                let result = MaybeVector::map2(x, y, |x, y| {
                    let xi = x.as_i16().unwrap_or(0);
                    let yi = y.as_i16().unwrap_or(0);

                    // Try cached lookup for common sizes
                    if w == 6 && h == 5 {
                        // Player hitbox - but we need to account for the offset
                        // solid_player expects position without hitbox offset
                        if let Some(v) = collision_cache.solid_player(xi - 1, yi - 3) {
                            return v;
                        }
                    } else if w == 1 && h == 1 {
                        if let Some(v) = collision_cache.solid_1x1(xi, yi) {
                            return v;
                        }
                    } else if w == 8 && h == 8 {
                        if let Some(v) = collision_cache.solid_8x8(xi, yi) {
                            return v;
                        }
                    }

                    // Fall back to computation
                    collision_cache.solid_at(&cart_data, xi, yi, w, h).unwrap_or(false)
                });
                Ok(vec![(state, Value::Bool(result))])
            }
            _ => tile_flag_at_computed(&cart_data, &collision_cache, state, &args),
        }
    }
}

/// Fallback computation for tile_flag_at when cache doesn't apply
fn tile_flag_at_computed(
    cart_data: &cart_data::CartData,
    collision_cache: &CollisionCache,
    state: State,
    args: &[Value],
) -> Result<Vec<(State, Value)>> {
    // This path handles non-standard cases
    match (&args[0], &args[1], &args[2], &args[3], &args[4]) {
        (
            Value::Number(x),
            Value::Number(y),
            Value::Number(w),
            Value::Number(h),
            Value::Number(flag),
        ) => {
            // All 5-way map
            let x_vals: Vec<Pico8Num> = match x {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.clone(),
            };
            let y_vals: Vec<Pico8Num> = match y {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.clone(),
            };
            let w_vals: Vec<Pico8Num> = match w {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.clone(),
            };
            let h_vals: Vec<Pico8Num> = match h {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.clone(),
            };
            let flag_vals: Vec<Pico8Num> = match flag {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.clone(),
            };

            // Broadcast to common length
            let len = [x_vals.len(), y_vals.len(), w_vals.len(), h_vals.len(), flag_vals.len()]
                .into_iter()
                .max()
                .unwrap();

            let get_or_last = |v: &[Pico8Num], i: usize| v.get(i).copied().unwrap_or(*v.last().unwrap());

            let results: Vec<bool> = (0..len)
                .map(|i| {
                    let xi = get_or_last(&x_vals, i).as_i16().unwrap_or(0);
                    let yi = get_or_last(&y_vals, i).as_i16().unwrap_or(0);
                    let wi = get_or_last(&w_vals, i).as_i16().unwrap_or(1);
                    let hi = get_or_last(&h_vals, i).as_i16().unwrap_or(1);
                    let flagi = get_or_last(&flag_vals, i).as_i16().unwrap_or(0);

                    if flagi == 0 {
                        collision_cache.solid_at(cart_data, xi, yi, wi, hi).unwrap_or(false)
                    } else {
                        // For other flags, we'd need a different cache or compute
                        // For now, just return false (not ideal but allows testing)
                        false
                    }
                })
                .collect();

            let result = if results.len() == 1 {
                Value::Bool(MaybeVector::Scalar(results[0]))
            } else {
                Value::Bool(MaybeVector::Vector(results))
            };
            Ok(vec![(state, result)])
        }
        _ => Err(anyhow!("tile_flag_at: all arguments must be numbers")),
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

    // Create collision cache for room (1, 0) - hardcoded for now
    let collision_cache = std::sync::Arc::new(
        CollisionCache::new(&cart_data, 1, 0).expect("Failed to create collision cache")
    );
    eprintln!("[game_runner] Created collision cache for room (1, 0)");

    fixed_env.add_builtin("mget", make_builtin_mget(cart_data.clone()));
    fixed_env.add_builtin("fget", make_builtin_fget(cart_data.clone()));
    fixed_env.add_builtin("tile_flag_at", make_builtin_tile_flag_at(cart_data, collision_cache));
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

/// Inject the tile_flag_at builtin into a state's global_env, replacing the Lua version.
/// This should be called after Lua init has completed.
pub fn inject_tile_flag_at_builtin(state: &mut State) {
    let builtin_name = "tile_flag_at".to_string();
    let heap_id = state.heap.alloc();
    state.heap.set(heap_id, HeapValue::BuiltinFun(builtin_name.clone()));
    state.global_env.insert(builtin_name, heap_id);
}
