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

        _ => Err(anyhow!("Unsupported binary op: {:?} {:?} {:?}", l, op, r)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::pico8_num::Pico8Num;

    // Helper to create a scalar number
    fn num(n: i16) -> Value {
        Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(n)))
    }

    // Helper to create a scalar boolean
    fn bool_val(b: bool) -> Value {
        Value::Bool(MaybeVector::Scalar(b))
    }

    // Helper to create an interval
    fn interval(low: i16, high: i16) -> Value {
        Value::NumberInterval(MaybeVector::Scalar(Pico8NumInterval::new(
            Pico8Num::from_i16(low),
            Pico8Num::from_i16(high),
        )))
    }

    // ======== Unary Op: Not ========

    #[test]
    fn test_not_true() {
        let result = interpret_not(&bool_val(true)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_not_false() {
        let result = interpret_not(&bool_val(false)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_not_unknown_bool() {
        let result = interpret_not(&Value::UnknownBool).unwrap();
        assert_eq!(result, Value::UnknownBool);
    }

    #[test]
    #[should_panic(expected = "Unsupported value for not")]
    fn test_not_number_panics() {
        let _ = interpret_not(&num(5));
    }

    #[test]
    fn test_not_vector() {
        let v = Value::Bool(MaybeVector::Vector(vec![true, false, true]));
        let result = interpret_not(&v).unwrap();
        assert_eq!(
            result,
            Value::Bool(MaybeVector::Vector(vec![false, true, false]))
        );
    }

    // ======== Unary Op: Minus ========

    #[test]
    fn test_minus_positive_number() {
        let state = State::default();
        let result = interpret_unary_op(&state, UnaryOp::Minus, &num(5)).unwrap();
        assert_eq!(result, num(-5));
    }

    #[test]
    fn test_minus_negative_number() {
        let state = State::default();
        let result = interpret_unary_op(&state, UnaryOp::Minus, &num(-3)).unwrap();
        assert_eq!(result, num(3));
    }

    #[test]
    fn test_minus_zero() {
        let state = State::default();
        let result = interpret_unary_op(&state, UnaryOp::Minus, &num(0)).unwrap();
        assert_eq!(result, num(0));
    }

    #[test]
    fn test_minus_interval() {
        let state = State::default();
        // Negating [2, 5] gives [-5, -2]
        let result = interpret_unary_op(&state, UnaryOp::Minus, &interval(2, 5)).unwrap();
        assert_eq!(result, interval(-5, -2));
    }

    #[test]
    fn test_minus_interval_spanning_zero() {
        let state = State::default();
        // Negating [-3, 5] gives [-5, 3]
        let result = interpret_unary_op(&state, UnaryOp::Minus, &interval(-3, 5)).unwrap();
        assert_eq!(result, interval(-5, 3));
    }

    // ======== Unary Op: Hash (length) ========

    #[test]
    fn test_hash_string() {
        let state = State::default();
        let result =
            interpret_unary_op(&state, UnaryOp::Hash, &Value::String("hello".to_string())).unwrap();
        assert_eq!(result, num(5));
    }

    #[test]
    fn test_hash_empty_string() {
        let state = State::default();
        let result =
            interpret_unary_op(&state, UnaryOp::Hash, &Value::String("".to_string())).unwrap();
        assert_eq!(result, num(0));
    }

    // ======== Binary Op: Equality (TwoEqual) ========

    #[test]
    fn test_equal_numbers_same() {
        let result = interpret_binary_op(&num(5), BinaryOp::TwoEqual, &num(5)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_equal_numbers_different() {
        let result = interpret_binary_op(&num(5), BinaryOp::TwoEqual, &num(3)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_equal_bools_same() {
        let result =
            interpret_binary_op(&bool_val(true), BinaryOp::TwoEqual, &bool_val(true)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_equal_bools_different() {
        let result =
            interpret_binary_op(&bool_val(true), BinaryOp::TwoEqual, &bool_val(false)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_equal_strings_same() {
        let result = interpret_binary_op(
            &Value::String("foo".to_string()),
            BinaryOp::TwoEqual,
            &Value::String("foo".to_string()),
        )
        .unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_equal_strings_different() {
        let result = interpret_binary_op(
            &Value::String("foo".to_string()),
            BinaryOp::TwoEqual,
            &Value::String("bar".to_string()),
        )
        .unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_equal_nil_nil() {
        let result = interpret_binary_op(
            &Value::Nil(None),
            BinaryOp::TwoEqual,
            &Value::Nil(Some("test".to_string())),
        )
        .unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_equal_nil_number() {
        let result = interpret_binary_op(&Value::Nil(None), BinaryOp::TwoEqual, &num(0)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_equal_number_nil() {
        let result = interpret_binary_op(&num(0), BinaryOp::TwoEqual, &Value::Nil(None)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_equal_number_string() {
        let result = interpret_binary_op(
            &num(5),
            BinaryOp::TwoEqual,
            &Value::String("5".to_string()),
        )
        .unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_equal_bool_unknown_bool() {
        let result =
            interpret_binary_op(&bool_val(true), BinaryOp::TwoEqual, &Value::UnknownBool).unwrap();
        assert_eq!(result, Value::UnknownBool);
    }

    #[test]
    fn test_equal_unknown_bool_bool() {
        let result =
            interpret_binary_op(&Value::UnknownBool, BinaryOp::TwoEqual, &bool_val(true)).unwrap();
        assert_eq!(result, Value::UnknownBool);
    }

    #[test]
    fn test_equal_unknown_bool_unknown_bool() {
        let result =
            interpret_binary_op(&Value::UnknownBool, BinaryOp::TwoEqual, &Value::UnknownBool)
                .unwrap();
        assert_eq!(result, Value::UnknownBool);
    }

    #[test]
    fn test_equal_unknown_bool_number() {
        let result =
            interpret_binary_op(&Value::UnknownBool, BinaryOp::TwoEqual, &num(5)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_equal_interval_anything() {
        // Intervals can't be compared for equality in general - always false
        let result = interpret_binary_op(&interval(1, 5), BinaryOp::TwoEqual, &num(3)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    // ======== Binary Op: Not Equal (TildeEqual) ========

    #[test]
    fn test_not_equal_numbers_same() {
        let result = interpret_binary_op(&num(5), BinaryOp::TildeEqual, &num(5)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_not_equal_numbers_different() {
        let result = interpret_binary_op(&num(5), BinaryOp::TildeEqual, &num(3)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_not_equal_unknown_bool() {
        let result =
            interpret_binary_op(&Value::UnknownBool, BinaryOp::TildeEqual, &bool_val(true))
                .unwrap();
        assert_eq!(result, Value::UnknownBool);
    }

    // ======== Binary Op: String Concatenation (TwoDots) ========

    #[test]
    fn test_concat_strings() {
        let result = interpret_binary_op(
            &Value::String("hello".to_string()),
            BinaryOp::TwoDots,
            &Value::String(" world".to_string()),
        )
        .unwrap();
        assert_eq!(result, Value::String("hello world".to_string()));
    }

    #[test]
    fn test_concat_string_number() {
        let result = interpret_binary_op(
            &Value::String("count: ".to_string()),
            BinaryOp::TwoDots,
            &num(42),
        )
        .unwrap();
        assert_eq!(result, Value::String("count: 42".to_string()));
    }

    #[test]
    fn test_concat_number_string() {
        let result = interpret_binary_op(
            &num(42),
            BinaryOp::TwoDots,
            &Value::String(" items".to_string()),
        )
        .unwrap();
        assert_eq!(result, Value::String("42 items".to_string()));
    }

    // ======== Binary Op: Arithmetic on Numbers ========

    #[test]
    fn test_add_numbers() {
        let result = interpret_binary_op(&num(3), BinaryOp::Plus, &num(4)).unwrap();
        assert_eq!(result, num(7));
    }

    #[test]
    fn test_subtract_numbers() {
        let result = interpret_binary_op(&num(10), BinaryOp::Minus, &num(4)).unwrap();
        assert_eq!(result, num(6));
    }

    #[test]
    fn test_multiply_numbers() {
        let result = interpret_binary_op(&num(3), BinaryOp::Star, &num(4)).unwrap();
        assert_eq!(result, num(12));
    }

    #[test]
    fn test_divide_numbers() {
        let result = interpret_binary_op(&num(12), BinaryOp::Slash, &num(4)).unwrap();
        assert_eq!(result, num(3));
    }

    #[test]
    fn test_modulo_numbers() {
        let result = interpret_binary_op(&num(10), BinaryOp::Percent, &num(3)).unwrap();
        assert_eq!(result, num(1));
    }

    // ======== Binary Op: Arithmetic on Intervals ========

    #[test]
    fn test_add_intervals() {
        // [1, 3] + [2, 4] = [3, 7]
        let result = interpret_binary_op(&interval(1, 3), BinaryOp::Plus, &interval(2, 4)).unwrap();
        assert_eq!(result, interval(3, 7));
    }

    #[test]
    fn test_subtract_intervals() {
        // [5, 10] - [2, 3] = [2, 8] (5-3=2, 10-2=8)
        let result =
            interpret_binary_op(&interval(5, 10), BinaryOp::Minus, &interval(2, 3)).unwrap();
        assert_eq!(result, interval(2, 8));
    }

    // ======== Binary Op: Mixed Number/Interval arithmetic ========

    #[test]
    fn test_add_number_interval() {
        // 5 + [2, 4] = [7, 9]
        let result = interpret_binary_op(&num(5), BinaryOp::Plus, &interval(2, 4)).unwrap();
        assert_eq!(result, interval(7, 9));
    }

    #[test]
    fn test_add_interval_number() {
        // [2, 4] + 5 = [7, 9]
        let result = interpret_binary_op(&interval(2, 4), BinaryOp::Plus, &num(5)).unwrap();
        assert_eq!(result, interval(7, 9));
    }

    #[test]
    fn test_subtract_number_interval() {
        // 10 - [2, 4] = [6, 8]
        let result = interpret_binary_op(&num(10), BinaryOp::Minus, &interval(2, 4)).unwrap();
        assert_eq!(result, interval(6, 8));
    }

    #[test]
    fn test_subtract_interval_number() {
        // [5, 10] - 3 = [2, 7]
        let result = interpret_binary_op(&interval(5, 10), BinaryOp::Minus, &num(3)).unwrap();
        assert_eq!(result, interval(2, 7));
    }

    // ======== Binary Op: Comparison on Numbers ========

    #[test]
    fn test_less_than_true() {
        let result = interpret_binary_op(&num(3), BinaryOp::LessThan, &num(5)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_less_than_false() {
        let result = interpret_binary_op(&num(5), BinaryOp::LessThan, &num(3)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_less_than_equal() {
        let result = interpret_binary_op(&num(5), BinaryOp::LessThan, &num(5)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_greater_than_true() {
        let result = interpret_binary_op(&num(5), BinaryOp::GreaterThan, &num(3)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_greater_than_false() {
        let result = interpret_binary_op(&num(3), BinaryOp::GreaterThan, &num(5)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_less_than_or_equal_true_lt() {
        let result = interpret_binary_op(&num(3), BinaryOp::LessThanEqual, &num(5)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_less_than_or_equal_true_eq() {
        let result = interpret_binary_op(&num(5), BinaryOp::LessThanEqual, &num(5)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_less_than_or_equal_false() {
        let result = interpret_binary_op(&num(5), BinaryOp::LessThanEqual, &num(3)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    #[test]
    fn test_greater_than_or_equal_true_gt() {
        let result = interpret_binary_op(&num(5), BinaryOp::GreaterThanEqual, &num(3)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_greater_than_or_equal_true_eq() {
        let result = interpret_binary_op(&num(5), BinaryOp::GreaterThanEqual, &num(5)).unwrap();
        assert_eq!(result, bool_val(true));
    }

    #[test]
    fn test_greater_than_or_equal_false() {
        let result = interpret_binary_op(&num(3), BinaryOp::GreaterThanEqual, &num(5)).unwrap();
        assert_eq!(result, bool_val(false));
    }

    // ======== Vector operations ========

    #[test]
    fn test_add_vectors() {
        let a = Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(3),
        ]));
        let b = Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(10),
            Pico8Num::from_i16(20),
            Pico8Num::from_i16(30),
        ]));
        let result = interpret_binary_op(&a, BinaryOp::Plus, &b).unwrap();
        assert_eq!(
            result,
            Value::Number(MaybeVector::Vector(vec![
                Pico8Num::from_i16(11),
                Pico8Num::from_i16(22),
                Pico8Num::from_i16(33),
            ]))
        );
    }

    #[test]
    fn test_scalar_vector_broadcast() {
        // Scalar + Vector = Vector (broadcasting)
        let scalar = num(10);
        let vector = Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(3),
        ]));
        let result = interpret_binary_op(&scalar, BinaryOp::Plus, &vector).unwrap();
        assert_eq!(
            result,
            Value::Number(MaybeVector::Vector(vec![
                Pico8Num::from_i16(11),
                Pico8Num::from_i16(12),
                Pico8Num::from_i16(13),
            ]))
        );
    }

    #[test]
    fn test_compare_vectors() {
        let a = Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(5),
            Pico8Num::from_i16(3),
        ]));
        let b = Value::Number(MaybeVector::Vector(vec![
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(4),
            Pico8Num::from_i16(3),
        ]));
        let result = interpret_binary_op(&a, BinaryOp::LessThan, &b).unwrap();
        // 1<2=true, 5<4=false, 3<3=false
        assert_eq!(
            result,
            Value::Bool(MaybeVector::Vector(vec![true, false, false]))
        );
    }

    // ======== Error cases ========

    #[test]
    fn test_unsupported_binary_op() {
        // String - String is not supported
        let result = interpret_binary_op(
            &Value::String("a".to_string()),
            BinaryOp::Minus,
            &Value::String("b".to_string()),
        );
        assert!(result.is_err());
    }
}
