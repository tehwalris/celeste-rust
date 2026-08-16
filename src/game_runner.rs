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
    // PICO-8's `add` RETURNS the value it appended; this returned nil.
    // Nothing in the cart uses the result - checked - so the difference is
    // latent, but "not reachable today" is the reasoning that left `foreach`
    // wrong for months.
    Ok(vec![(state, args[1].clone())])
}

fn builtin_new_unknown_boolean(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if !args.is_empty() {
        return Err(anyhow!("__new_unknown_boolean takes no arguments"));
    }
    Ok(vec![(state, Value::UnknownBool)])
}

/// `__widen_rem(v)`: assert `v` lies in the player-rem interval `[-0.5, 0.5)`
/// and return that whole interval, as a scalar (equal across lanes - that is
/// the point: a widened rem column stops distinguishing rows). This is
/// `make_state_abstract`'s rem widening exposed to the program, so a rewrite
/// can schedule the abstraction before the frame boundary. The containment
/// check is the runtime guard for a misplaced insertion.
fn builtin_widen_rem(state: State, args: Vec<Value>) -> Result<Vec<(State, Value)>> {
    if args.len() != 1 {
        return Err(anyhow!("__widen_rem requires 1 argument"));
    }
    let half = Pico8Num::from_parts(0, 0x8000);
    let wide = Pico8NumInterval::new(-half, half.next_smallest());
    let check_number = |n: &Pico8Num| -> Result<()> {
        if wide.contains_number(*n) {
            Ok(())
        } else {
            Err(anyhow!("__widen_rem: value {:?} is not in [-0.5, 0.5)", n))
        }
    };
    let check_interval = |i: &Pico8NumInterval| -> Result<()> {
        if wide.contains_interval(i) {
            Ok(())
        } else {
            Err(anyhow!("__widen_rem: interval {:?} is not in [-0.5, 0.5)", i))
        }
    };
    match &args[0] {
        Value::Number(MaybeVector::Scalar(n)) => check_number(n)?,
        Value::Number(MaybeVector::Vector(nums)) => {
            for n in nums.iter() {
                check_number(n)?;
            }
        }
        Value::NumberInterval(MaybeVector::Scalar(i)) => check_interval(i)?,
        Value::NumberInterval(MaybeVector::Vector(intervals)) => {
            for i in intervals.iter() {
                check_interval(i)?;
            }
        }
        other => {
            return Err(anyhow!("__widen_rem: expected a number, got {:?}", other));
        }
    }
    Ok(vec![(state, Value::NumberInterval(MaybeVector::Scalar(wide)))])
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
    Ok(vec![(state, Value::Number(MaybeVector::vector(numbers)))])
}

fn builtin_flr(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(anyhow!("flr requires 1 argument"));
    }
    match &args[0] {
        Value::Number(nums) => {
            let result = nums.map(|n| n.flr());
            Ok(Value::Number(result))
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
            Ok(Value::Number(MaybeVector::Scalar(low_flr)))
        }
        Value::NumberInterval(MaybeVector::Vector(intervals)) => {
            // For a vector of intervals, each must have a single floor value
            let mut floors = Vec::with_capacity(intervals.len());
            for interval in intervals.iter() {
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
                Value::Number(MaybeVector::vector(floors))
            };
            Ok(result)
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
                    Value::Number(MaybeVector::vector(result_nums))
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
                return Ok(vec![(state, Value::NumberInterval(MaybeVector::vector(vec![])))]);
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
                    Value::NumberInterval(MaybeVector::vector(result_intervals))
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

/// Lift a numeric Value (Number or NumberInterval) to intervals, for the
/// builtins whose interval extension is exact (min/max/abs are monotone,
/// abs piecewise). Returns None for non-numeric values.
fn value_as_intervals(v: &Value) -> Option<MaybeVector<Pico8NumInterval>> {
    match v {
        Value::Number(nv) => Some(nv.map_to(|n| Pico8NumInterval::from_number(*n))),
        Value::NumberInterval(iv) => Some(iv.clone()),
        _ => None,
    }
}

/// abs over an interval: monotone on each side of 0; a zero-straddling
/// interval maps to [0, max(|lo|, |hi|)]. Uses Pico8Num::abs (saturating)
/// on the endpoints, so the 0x8000.0000 edge behaves exactly like the
/// concrete op.
fn interval_abs(iv: &Pico8NumInterval) -> Pico8NumInterval {
    let zero = Pico8Num::from_i16(0);
    if iv.low >= zero {
        *iv
    } else if iv.high <= zero {
        Pico8NumInterval::new(iv.high.abs(), iv.low.abs())
    } else {
        Pico8NumInterval::new(zero, iv.low.abs().max(iv.high.abs()))
    }
}

fn builtin_min(args: &[Value]) -> Result<Value> {
    if args.len() != 2 { return Err(anyhow!("min requires 2 arguments")); }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => {
            let result = MaybeVector::map2(a, b, |a, b| (*a).min(*b));
            Ok(Value::Number(result))
        }
        // The interval extension of min is exact: min is monotone in
        // both arguments, so min(A, B) = [min(lo), min(hi)] pointwise.
        (a, b) => match (value_as_intervals(a), value_as_intervals(b)) {
            (Some(a), Some(b)) => Ok(Value::NumberInterval(MaybeVector::map2(
                &a,
                &b,
                |a, b| Pico8NumInterval::new(a.low.min(b.low), a.high.min(b.high)),
            ))),
            _ => Err(anyhow!("min: arguments must be numbers")),
        },
    }
}

fn builtin_max(args: &[Value]) -> Result<Value> {
    if args.len() != 2 { return Err(anyhow!("max requires 2 arguments")); }
    match (&args[0], &args[1]) {
        (Value::Number(a), Value::Number(b)) => {
            let result = MaybeVector::map2(a, b, |a, b| (*a).max(*b));
            Ok(Value::Number(result))
        }
        // Exact for the same reason as min.
        (a, b) => match (value_as_intervals(a), value_as_intervals(b)) {
            (Some(a), Some(b)) => Ok(Value::NumberInterval(MaybeVector::map2(
                &a,
                &b,
                |a, b| Pico8NumInterval::new(a.low.max(b.low), a.high.max(b.high)),
            ))),
            _ => Err(anyhow!("max: arguments must be numbers")),
        },
    }
}

fn builtin_abs(args: &[Value]) -> Result<Value> {
    if args.len() != 1 { return Err(anyhow!("abs requires 1 argument")); }
    match &args[0] {
        Value::Number(nums) => {
            let result = nums.map(|n| n.abs());
            Ok(Value::Number(result))
        }
        Value::NumberInterval(ivs) => {
            Ok(Value::NumberInterval(ivs.map(interval_abs)))
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

/// Registered through `add_pure_builtin`: a function of its arguments alone.
/// The cart data is captured at construction and the game never calls `mset`,
/// so the map it reads is immutable for the lifetime of the environment.
fn make_builtin_mget(
    cart_data: std::sync::Arc<cart_data::CartData>,
) -> impl Fn(&[Value]) -> Result<Value> {
    move |args: &[Value]| {
        if args.len() != 2 { return Err(anyhow!("mget requires 2 arguments")); }
        match (&args[0], &args[1]) {
            (Value::Number(x), Value::Number(y)) => {
                let result = MaybeVector::map2(x, y, |x, y| {
                    Pico8Num::from_i16(cart_data.mget(*x, *y).expect("mget failed") as i16)
                });
                Ok(Value::Number(result))
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
///
/// Registered through `add_pure_builtin`: a function of its arguments alone.
/// The cart data and the collision cache are captured at construction and are
/// immutable for the lifetime of the environment - and the cache is built for
/// the configured `start_room()`, which is where the whole search happens.
/// The real Lua `tile_flag_at` reads the `room` global; this replacement
/// bakes the room in, so if the search ever crosses a room boundary the
/// builtin must grow a room argument (and lose this registration) rather
/// than serve stale collision data. See `inject_tile_flag_at_builtin`.
fn make_builtin_tile_flag_at(
    cart_data: std::sync::Arc<cart_data::CartData>,
    collision_cache: std::sync::Arc<CollisionCache>,
) -> impl Fn(&[Value]) -> Result<Value> {
    move |args: &[Value]| {
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
            return tile_flag_at_computed(&cart_data, &collision_cache, args);
        }

        // Extract scalar w, h (these are typically constant)
        let (w, h) = match (&args[2], &args[3]) {
            (Value::Number(MaybeVector::Scalar(w)), Value::Number(MaybeVector::Scalar(h))) => {
                (w.as_i16().ok_or_else(|| anyhow!("w must be integer"))?,
                 h.as_i16().ok_or_else(|| anyhow!("h must be integer"))?)
            }
            _ => return tile_flag_at_computed(&cart_data, &collision_cache, args),
        };

        // Handle x, y which may be vectors.
        //
        // Fused deliberately: converting x and y to i16 through
        // `as_i16_elements` first cost two intermediate Vec allocations,
        // two uniformity scans inside `MaybeVector::vector`, and two extra
        // round-trips through memory before `map2` even started - and this
        // builtin is the single largest cluster in the profile (13.9% of
        // the search: the closure, as_i16_elements, tile_flag_at_computed
        // and mget together). One pass, one allocation, conversion folded
        // into the lookup.
        match (&args[0], &args[1]) {
            (Value::Number(x), Value::Number(y)) => {
                // (w, h) is the same for every lane of a call, so the map
                // and its offset are chosen once here rather than re-tested
                // per lane inside the loop below.
                let map = collision_cache.solid_map(w, h);
                let lookup = |xn: &Pico8Num, yn: &Pico8Num| -> Result<bool> {
                    let xi = xn.as_i16().ok_or_else(|| {
                        anyhow!("tile_flag_at: x must be an integer, got {:?}", xn)
                    })?;
                    let yi = yn.as_i16().ok_or_else(|| {
                        anyhow!("tile_flag_at: y must be an integer, got {:?}", yn)
                    })?;
                    if let Some((map, dx, dy)) = map {
                        if let Some(v) = map.get(xi + dx, yi + dy) {
                            return Ok(v);
                        }
                    }
                    // Outside the precomputed range, or a hitbox size with
                    // no map: compute it.
                    Ok(collision_cache.solid_at(&cart_data, xi, yi, w, h).unwrap_or(false))
                };
                let result = match (x, y) {
                    (MaybeVector::Scalar(xn), MaybeVector::Scalar(yn)) => {
                        MaybeVector::Scalar(lookup(xn, yn)?)
                    }
                    (MaybeVector::Vector(xs), MaybeVector::Vector(ys)) => {
                        if xs.len() != ys.len() {
                            return Err(anyhow!(
                                "tile_flag_at: x and y have different lane counts ({} vs {})",
                                xs.len(),
                                ys.len()
                            ));
                        }
                        let mut out = Vec::with_capacity(xs.len());
                        for (xn, yn) in xs.iter().zip(ys.iter()) {
                            out.push(lookup(xn, yn)?);
                        }
                        MaybeVector::vector(out)
                    }
                    (MaybeVector::Scalar(xn), MaybeVector::Vector(ys)) => {
                        let mut out = Vec::with_capacity(ys.len());
                        for yn in ys.iter() {
                            out.push(lookup(xn, yn)?);
                        }
                        MaybeVector::vector(out)
                    }
                    (MaybeVector::Vector(xs), MaybeVector::Scalar(yn)) => {
                        let mut out = Vec::with_capacity(xs.len());
                        for xn in xs.iter() {
                            out.push(lookup(xn, yn)?);
                        }
                        MaybeVector::vector(out)
                    }
                };
                Ok(Value::Bool(result))
            }
            _ => tile_flag_at_computed(&cart_data, &collision_cache, args),
        }
    }
}

/// Fallback computation for tile_flag_at when cache doesn't apply
fn tile_flag_at_computed(
    cart_data: &cart_data::CartData,
    collision_cache: &CollisionCache,
    args: &[Value],
) -> Result<Value> {
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
                MaybeVector::Vector(v) => v.as_ref().clone(),
            };
            let y_vals: Vec<Pico8Num> = match y {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.as_ref().clone(),
            };
            let w_vals: Vec<Pico8Num> = match w {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.as_ref().clone(),
            };
            let h_vals: Vec<Pico8Num> = match h {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.as_ref().clone(),
            };
            let flag_vals: Vec<Pico8Num> = match flag {
                MaybeVector::Scalar(n) => vec![*n],
                MaybeVector::Vector(v) => v.as_ref().clone(),
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
                Value::Bool(MaybeVector::vector(results))
            };
            Ok(result)
        }
        _ => Err(anyhow!("tile_flag_at: all arguments must be numbers")),
    }
}

/// The room the search starts in, from `CELESTE_START_ROOM` ("x,y"),
/// default (1, 0). Drives the `_init` load_room substitution
/// (`apply_start_room`), the collision cache room, the `sin` builtin
/// registration, and the checkpoint config fingerprint - all four must
/// agree, which is why this is the single source of truth.
pub fn start_room() -> (i16, i16) {
    static ROOM: std::sync::OnceLock<(i16, i16)> = std::sync::OnceLock::new();
    *ROOM.get_or_init(|| match std::env::var("CELESTE_START_ROOM") {
        Ok(s) => {
            let (x, y) = s
                .split_once(',')
                .unwrap_or_else(|| panic!("CELESTE_START_ROOM must be \"x,y\", got {:?}", s));
            (
                x.trim().parse().expect("CELESTE_START_ROOM x must be an integer"),
                y.trim().parse().expect("CELESTE_START_ROOM y must be an integer"),
            )
        }
        Err(_) => (1, 0),
    })
}

/// The `room.x` value that means "won" for the configured start room: the
/// room the player lands in after exiting. Progression in this cart is
/// `room.x + 1` along a map row (`next_room` only wraps at x == 7, which no
/// supported start room reaches - asserted). Comparing `room.x` alone is
/// enough because a same-row exit never changes `room.y`.
pub fn win_room_x() -> i16 {
    let (x, _y) = start_room();
    assert!(
        x < 7,
        "win_room_x: start room x={} would wrap to the next map row",
        x
    );
    x + 1
}

/// Directory-name stem for per-room checkpoint trees under a base dir:
/// level 0 lives at `<base>/<stem>`, level k at `<base>/<stem>-k<k>`.
/// The default room keeps its historical name "room1" so existing
/// checkpoint trees on disk stay addressable; other rooms get "room<x><y>"
/// (e.g. "room00").
pub fn room_dir_stem() -> String {
    let (x, y) = start_room();
    if (x, y) == (1, 0) {
        "room1".to_string()
    } else {
        format!("room{}{}", x, y)
    }
}

/// Point the game's `_init` at the configured start room. Strict: the
/// checked-in lua must contain the default call exactly once, so a source
/// edit can never silently disable the substitution.
pub fn apply_start_room(game_lua: &str) -> Result<String> {
    const PAT: &str = "load_room(1, 0)";
    let count = game_lua.matches(PAT).count();
    if count != 1 {
        return Err(anyhow!(
            "expected exactly one {:?} in the game lua, found {}",
            PAT,
            count
        ));
    }
    let (x, y) = start_room();
    Ok(game_lua.replacen(PAT, &format!("load_room({}, {})", x, y), 1))
}

/// PICO-8 `sin` (see `Pico8Num::pico8_sin`). Interval arguments come from
/// the fruit-off widening (plans/room00-plan.md): at non-exact refinement
/// levels the fruit's bob counter is an unknown-within-period interval, so
/// sin over it soundly returns the FULL range [-1, 1] - the coarse levels
/// over-approximate the bob and the exact level (concrete off) resolves it,
/// same contract as the rem ladder.
fn builtin_sin(args: &[Value]) -> Result<Value> {
    if args.len() != 1 {
        return Err(anyhow!("sin requires 1 argument"));
    }
    match &args[0] {
        Value::Number(nums) => Ok(Value::Number(nums.map(|n| n.pico8_sin()))),
        Value::NumberInterval(ivs) => {
            let full = crate::pico8_num::Pico8NumInterval::new(
                Pico8Num::from_i16(-1),
                Pico8Num::from_i16(1),
            );
            Ok(Value::NumberInterval(ivs.map(|_| full)))
        }
        other => Err(anyhow!(
            "sin: unsupported argument (neither number nor interval): {:?}",
            other
        )),
    }
}

pub fn create_fixed_env_with_builtins() -> FixedEnv {
    let mut fixed_env = FixedEnv::new();
    fixed_env.add_builtin("__print", builtin_print);
    fixed_env.add_builtin("__new_unknown_boolean", builtin_new_unknown_boolean);
    fixed_env.add_builtin("__widen_rem", builtin_widen_rem);
    fixed_env.add_builtin("__new_vector", builtin_new_vector);
    fixed_env.add_builtin("__array_table_drop_last", builtin_array_table_drop_last);
    fixed_env.add_builtin("error", builtin_error);
    fixed_env.add_pure_builtin("min", builtin_min);
    fixed_env.add_pure_builtin("max", builtin_max);
    fixed_env.add_pure_builtin("abs", builtin_abs);
    fixed_env.add_pure_builtin("flr", builtin_flr);
    fixed_env.add_builtin("__split_by_flr", builtin_split_by_flr);
    fixed_env.add_builtin("add", builtin_add);
    fixed_env.add_builtin("print", builtin_print);
    fixed_env
}

pub fn create_fixed_env_with_game_builtins() -> FixedEnv {
    let mut fixed_env = create_fixed_env_with_builtins();
    let (room_x, room_y) = start_room();
    // `sin` is native for every room. (It used to be conditional on the
    // start room to keep the pre-existing room-(1,0) row hashes valid;
    // the 2026-08 hash-breaking batch bumped the checkpoint FORMAT_VERSION
    // and re-derived both rooms' universes, so the conditional is gone -
    // it also meant the default room's program crashed the moment a replay
    // crossed into a fruit room.)
    fixed_env.add_pure_builtin("sin", builtin_sin);
    let cart_data = std::sync::Arc::new(
        cart_data::CartData::load("cart").expect("Failed to load cart data")
    );

    let collision_cache = std::sync::Arc::new(
        CollisionCache::new(&cart_data, room_x, room_y)
            .expect("Failed to create collision cache")
    );
    eprintln!("[game_runner] Created collision cache for room ({}, {})", room_x, room_y);

    fixed_env.add_pure_builtin("mget", make_builtin_mget(cart_data.clone()));
    fixed_env.add_builtin("fget", make_builtin_fget(cart_data.clone()));
    fixed_env.add_pure_builtin("tile_flag_at", make_builtin_tile_flag_at(cart_data, collision_cache));
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

#[cfg(test)]
mod tests {
    use super::apply_start_room;

    // start_room() is a process-wide OnceLock, so tests can only exercise
    // the default (1,0) configuration; the non-default path is exercised by
    // the room-(0,0) pipeline runs.
    #[test]
    fn win_and_dir_stem_for_default_room() {
        assert_eq!(super::win_room_x(), 2);
        assert_eq!(super::room_dir_stem(), "room1");
    }

    // These run with the default start room (1,0), where the substitution
    // must be an exact identity - and the strictness must still hold.
    #[test]
    fn apply_start_room_is_identity_for_default_room() {
        let src = "function _init()\n\tload_room(1, 0)\nend\n";
        assert_eq!(apply_start_room(src).unwrap(), src);
    }

    #[test]
    fn apply_start_room_rejects_missing_or_duplicated_call() {
        assert!(apply_start_room("load_room(7,3)").is_err());
        assert!(apply_start_room("load_room(1, 0) load_room(1, 0)").is_err());
    }
}
