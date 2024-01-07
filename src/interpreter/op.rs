use crate::pico8_num::Pico8Num;

use super::{
    state::State,
    value::{MaybeVector, Value},
};

fn interpret_unary_op(state: &State, op: &str, v: &Value) -> Value {
    match (op, v) {
        ("-", Value::Number(v)) => Value::Number(v.map(|v| -*v)),
        ("not", Value::Bool(v)) => Value::Bool(v.map(|v| !v)),
        ("not", Value::UnknownBool) => Value::UnknownBool,
        ("#", Value::String(v)) => Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(
            v.len().try_into().unwrap(),
        ))),
        ("#", Value::Pointer(heap_id)) => {
            todo!()
        }
        (op, v) => panic!("Unsupported unary op: {} {:?}", op, v),
    }
}
