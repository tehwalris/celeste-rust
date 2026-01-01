use anyhow::Result;

use super::{
    state::State,
    value::{HeapValue, MaybeVector, Value},
};
use crate::{
    ir::{BinaryOp, UnaryOp},
    pico8_num::{Pico8Num, Pico8NumInterval},
};

fn interpret_not(v: &Value) -> Result<Value> {
    match v {
        Value::Bool(v) => Ok(Value::Bool(v.map(|v| !v))),
        Value::UnknownBool => Ok(Value::UnknownBool),
        v => panic!("Unsupported value for not: {:?}", v),
    }
}

pub fn interpret_unary_op(state: &State, op: UnaryOp, v: &Value) -> Result<Value> {
    match (op, v) {
        (UnaryOp::Minus, Value::Number(v)) => Ok(Value::Number(v.map(|v| -*v))),
        (UnaryOp::Minus, Value::NumberInterval(v)) => Ok(Value::NumberInterval(v.map(|v| {
            // Negating an interval [a,b] gives [-b, -a]
            Pico8NumInterval::new(-v.high, -v.low)
        }))),
        (UnaryOp::Not, v) => interpret_not(v),
        (UnaryOp::Hash, Value::String(v)) => Ok(Value::Number(MaybeVector::Scalar(
            Pico8Num::from_i16(v.len().try_into().unwrap()),
        ))),
        (UnaryOp::Hash, Value::Pointer(heap_id)) => {
            match state.heap.get(*heap_id) {
                HeapValue::ArrayTable(items) => Ok(Value::Number(MaybeVector::Scalar(
                    Pico8Num::from_i16(items.len().try_into().unwrap()),
                ))),
                HeapValue::ObjectTable(_) => {
                    // In Lua, # on object tables returns 0 (no array part)
                    Ok(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(0))))
                }
                HeapValue::UnknownTable => {
                    // Empty/unknown tables have length 0
                    Ok(Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(0))))
                }
                other => Err(anyhow!("Hash operator on non-table pointer: {:?}", other)),
            }
        }
        _ => Err(anyhow!("Unsupported unary op: {:?} {:?}", op, v)),
    }
}

/// Helper to lift a number to an interval
fn lift_to_interval(v: &MaybeVector<Pico8Num>) -> MaybeVector<Pico8NumInterval> {
    v.map_to(|n| Pico8NumInterval::from_number(*n))
}
pub fn interpret_binary_op(l: &Value, op: BinaryOp, r: &Value) -> Result<Value> {
    let sb = |b| Ok(Value::Bool(MaybeVector::Scalar(b)));

    // Handle mixed Number/NumberInterval by lifting Number to NumberInterval
    match (l, op, r) {
        // Number op NumberInterval -> lift to interval
        (Value::Number(l), op, Value::NumberInterval(r))
            if matches!(op, BinaryOp::Plus | BinaryOp::Minus) =>
        {
            interpret_binary_op(
                &Value::NumberInterval(lift_to_interval(l)),
                op,
                &Value::NumberInterval(r.clone()),
            )
        }
        // NumberInterval op Number -> lift to interval
        (Value::NumberInterval(l), op, Value::Number(r))
            if matches!(op, BinaryOp::Plus | BinaryOp::Minus) =>
        {
            interpret_binary_op(
                &Value::NumberInterval(l.clone()),
                op,
                &Value::NumberInterval(lift_to_interval(r)),
            )
        }

        (_, BinaryOp::TildeEqual, _) => {
            interpret_not(&interpret_binary_op(l, BinaryOp::TwoEqual, r)?)
        }

        // Number == _
        (Value::Number(l), BinaryOp::TwoEqual, Value::Number(r)) => {
            Ok(Value::Bool(MaybeVector::map2(l, r, |l, r| l == r)))
        }
        (Value::Number(_), BinaryOp::TwoEqual, _) => sb(false),

        // NumberInterval == _ (can't compare intervals for equality in general)
        (Value::NumberInterval(_), BinaryOp::TwoEqual, _) => sb(false),

        // Bool == _
        (Value::Bool(l), BinaryOp::TwoEqual, Value::Bool(r)) => {
            Ok(Value::Bool(MaybeVector::map2(l, r, |l, r| l == r)))
        }
        (Value::Bool(_), BinaryOp::TwoEqual, Value::UnknownBool) => Ok(Value::UnknownBool),
        (Value::Bool(_), BinaryOp::TwoEqual, _) => sb(false),

        // UnknownBool == _
        (Value::UnknownBool, BinaryOp::TwoEqual, Value::UnknownBool) => Ok(Value::UnknownBool),
        (Value::UnknownBool, BinaryOp::TwoEqual, Value::Bool(_)) => Ok(Value::UnknownBool),
        (Value::UnknownBool, BinaryOp::TwoEqual, _) => sb(false),

        // String == _
        (Value::String(l), BinaryOp::TwoEqual, Value::String(r)) => sb(l == r),
        (Value::String(_), BinaryOp::TwoEqual, _) => sb(false),

        // Nil == _
        (Value::Nil(_), BinaryOp::TwoEqual, Value::Nil(_)) => sb(true),
        (Value::Nil(_), BinaryOp::TwoEqual, _) => sb(false),

        // Pointer == _
        (Value::Pointer(l), BinaryOp::TwoEqual, Value::Pointer(r)) => sb(l == r),
        (Value::Pointer(_), BinaryOp::TwoEqual, _) => sb(false),

        // _ .. _
        (Value::String(l), BinaryOp::TwoDots, Value::String(r)) => {
            Ok(Value::String(format!("{}{}", l, r)))
        }
        (Value::String(l), BinaryOp::TwoDots, Value::Number(MaybeVector::Scalar(r))) => {
            Ok(Value::String(format!("{}{}", l, r.as_i16_or_err()?)))
        }
        (Value::Number(MaybeVector::Scalar(l)), BinaryOp::TwoDots, Value::String(r)) => {
            Ok(Value::String(format!("{}{}", l.as_i16_or_err()?, r)))
        }

        // Arithmetic operations on numbers
        (Value::Number(l), BinaryOp::Plus, Value::Number(r)) => {
            Ok(Value::Number(MaybeVector::map2(l, r, |l, r| *l + *r)))
        }
        (Value::Number(l), BinaryOp::Minus, Value::Number(r)) => {
            Ok(Value::Number(MaybeVector::map2(l, r, |l, r| *l - *r)))
        }
        (Value::Number(l), BinaryOp::Star, Value::Number(r)) => {
            Ok(Value::Number(MaybeVector::map2(l, r, |l, r| *l * *r)))
        }
        (Value::Number(l), BinaryOp::Slash, Value::Number(r)) => {
            Ok(Value::Number(MaybeVector::map2(l, r, |l, r| *l / *r)))
        }
        (Value::Number(l), BinaryOp::Percent, Value::Number(r)) => {
            Ok(Value::Number(MaybeVector::map2(l, r, |l, r| *l % *r)))
        }

        // Arithmetic operations on intervals
        (Value::NumberInterval(l), BinaryOp::Plus, Value::NumberInterval(r)) => {
            Ok(Value::NumberInterval(MaybeVector::map2(l, r, |l, r| *l + *r)))
        }
        (Value::NumberInterval(l), BinaryOp::Minus, Value::NumberInterval(r)) => {
            Ok(Value::NumberInterval(MaybeVector::map2(l, r, |l, r| *l - *r)))
        }

        // Comparison operations on numbers
        (Value::Number(l), BinaryOp::LessThan, Value::Number(r)) => {
            Ok(Value::Bool(MaybeVector::map2(l, r, |l, r| l < r)))
        }
        (Value::Number(l), BinaryOp::GreaterThan, Value::Number(r)) => {
            Ok(Value::Bool(MaybeVector::map2(l, r, |l, r| l > r)))
        }
        (Value::Number(l), BinaryOp::LessThanEqual, Value::Number(r)) => {
            Ok(Value::Bool(MaybeVector::map2(l, r, |l, r| l <= r)))
        }
        (Value::Number(l), BinaryOp::GreaterThanEqual, Value::Number(r)) => {
            Ok(Value::Bool(MaybeVector::map2(l, r, |l, r| l >= r)))
        }

        // Comparison operations involving intervals produce UnknownBool
        // (interval compared to number or interval compared to interval)
        (Value::NumberInterval(_), BinaryOp::LessThan, Value::Number(_))
        | (Value::NumberInterval(_), BinaryOp::LessThan, Value::NumberInterval(_))
        | (Value::Number(_), BinaryOp::LessThan, Value::NumberInterval(_)) => {
            Ok(Value::UnknownBool)
        }
        (Value::NumberInterval(_), BinaryOp::GreaterThan, Value::Number(_))
        | (Value::NumberInterval(_), BinaryOp::GreaterThan, Value::NumberInterval(_))
        | (Value::Number(_), BinaryOp::GreaterThan, Value::NumberInterval(_)) => {
            Ok(Value::UnknownBool)
        }
        (Value::NumberInterval(_), BinaryOp::LessThanEqual, Value::Number(_))
        | (Value::NumberInterval(_), BinaryOp::LessThanEqual, Value::NumberInterval(_))
        | (Value::Number(_), BinaryOp::LessThanEqual, Value::NumberInterval(_)) => {
            Ok(Value::UnknownBool)
        }
        (Value::NumberInterval(_), BinaryOp::GreaterThanEqual, Value::Number(_))
        | (Value::NumberInterval(_), BinaryOp::GreaterThanEqual, Value::NumberInterval(_))
        | (Value::Number(_), BinaryOp::GreaterThanEqual, Value::NumberInterval(_)) => {
            Ok(Value::UnknownBool)
        }

        _ => Err(anyhow!("Unsupported binary op: {:?} {:?} {:?}", l, op, r)),
    }
}
