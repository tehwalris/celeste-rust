use std::fmt::Binary;

use crate::{
    ir::{BinaryOp, UnaryOp},
    pico8_num::Pico8Num,
};

use super::{
    state::State,
    value::{MaybeVector, Value},
};

fn interpret_not(v: &Value) -> Value {
    match v {
        Value::Bool(v) => Value::Bool(v.map(|v| !v)),
        Value::UnknownBool => Value::UnknownBool,
        v => panic!("Unsupported value for not: {:?}", v),
    }
}

pub fn interpret_unary_op(state: &State, op: UnaryOp, v: &Value) -> Value {
    match (op, v) {
        (UnaryOp::Minus, Value::Number(v)) => Value::Number(v.map(|v| -*v)),
        (UnaryOp::Not, v) => interpret_not(v),
        (UnaryOp::Hash, Value::String(v)) => Value::Number(MaybeVector::Scalar(
            Pico8Num::from_i16(v.len().try_into().unwrap()),
        )),
        (UnaryOp::Hash, Value::Pointer(heap_id)) => {
            todo!()
        }
        _ => panic!("Unsupported unary op: {:?} {:?}", op, v),
    }
}

/*
let rec interpret_binary_op_scalar (l : scalar_value) (op : string)
    (r : scalar_value) : scalar_value =
  let is_simple_value v =
    match v with
    | SNumber _ -> true
    | SNumberInterval _ -> false
    | SBool _ -> true
    | SUnknownBool -> false
    | SString _ -> true
    | SNil _ -> false
    | SPointer _ -> false
    | SNilPointer _ -> false
  in
  match (l, op, r) with
  | a, "==", b when is_simple_value a && is_simple_value b -> SBool (a = b)
  | a, "~=", b when is_simple_value a && is_simple_value b -> SBool (a <> b)
  | SNil _, "==", SNil _ -> SBool true
  | SNil _, "~=", SNil _ -> SBool false
  | a, "==", SNil _ when is_simple_value a -> SBool false
  | a, "~=", SNil _ when is_simple_value a -> SBool true
  | SNil _, "==", b when is_simple_value b -> SBool false
  | SNil _, "~=", b when is_simple_value b -> SBool true
  | SPointer l, "==", SPointer r -> SBool (l = r)
  | SPointer l, "~=", SPointer r -> SBool (l <> r)
  | a, "==", SPointer _ when is_simple_value a -> SBool false
  | a, "~=", SPointer _ when is_simple_value a -> SBool true
  | SPointer _, "==", b when is_simple_value b -> SBool false
  | SPointer _, "~=", b when is_simple_value b -> SBool true
  | SNil _, "==", SPointer _ -> SBool false
  | SNil _, "~=", SPointer _ -> SBool true
  | SPointer _, "==", SNil _ -> SBool false
  | SPointer _, "~=", SNil _ -> SBool true
  | SNumber l, "+", SNumber r -> SNumber (Pico_number.add l r)
  | SNumber l, "-", SNumber r -> SNumber (Pico_number.sub l r)
  | SNumber l, "*", SNumber r -> SNumber (Pico_number.mul l r)
  | SNumber l, "/", SNumber r -> SNumber (Pico_number.div l r)
  | SNumber l, "%", SNumber r -> SNumber (Pico_number.modulo l r)
  | SNumber l, "<", SNumber r -> SBool (Int32.compare l r < 0)
  | SNumber l, "<=", SNumber r -> SBool (Int32.compare l r <= 0)
  | SNumber l, ">", SNumber r -> SBool (Int32.compare l r > 0)
  | SNumber l, ">=", SNumber r -> SBool (Int32.compare l r >= 0)
  | SNumber l, op, SNumberInterval r ->
      let l = SNumberInterval (Pico_number_interval.of_number l) in
      let r = SNumberInterval r in
      interpret_binary_op_scalar l op r
  | SNumberInterval l, op, SNumber r ->
      let l = SNumberInterval l in
      let r = SNumberInterval (Pico_number_interval.of_number r) in
      interpret_binary_op_scalar l op r
  | SNumberInterval l, "+", SNumberInterval r ->
      SNumberInterval (Pico_number_interval.add l r)
  | SNumberInterval l, "-", SNumberInterval r ->
      SNumberInterval (Pico_number_interval.sub l r)
  | SString l, "..", SString r -> SString (l ^ r)
  | SString l, "..", SNumber r ->
      SString (l ^ Int.to_string @@ Pico_number.int_of r)
  | SNumber l, "..", SString r ->
      SString ((Int.to_string @@ Pico_number.int_of l) ^ r)
  | l, op, r ->
      failwith
      @@ Printf.sprintf "Unsupported binary op: %s %s %s" (show_scalar_value l)
           op (show_scalar_value r)
 */

fn interpret_binary_op(l: &Value, op: BinaryOp, r: &Value) -> Value {
    let v_false = Value::Bool(MaybeVector::Scalar(false));
    match (l, op, r) {
        (_, BinaryOp::TildeEqual, _) => {
            interpret_not(&interpret_binary_op(l, BinaryOp::TwoEqual, r))
        }

        // Number == _
        (Value::Number(_), BinaryOp::TwoEqual, Value::Number(_)) => todo!(),
        // TODO number == number interval
        (Value::Number(_), BinaryOp::TwoEqual, _) => v_false,

        // NumberInterval == _
        // TODO

        // Bool == _
        (Value::Bool(_), BinaryOp::TwoEqual, Value::Bool(_)) => todo!(),
        (Value::Bool(_), BinaryOp::TwoEqual, Value::UnknownBool) => todo!(),
        (Value::Bool(_), BinaryOp::TwoEqual, _) => v_false,

        // String == _
        (Value::String(_), BinaryOp::TwoEqual, Value::String(_)) => todo!(),
        (Value::String(_), BinaryOp::TwoEqual, _) => v_false,

        // Nil == _
        (Value::Nil(_), BinaryOp::TwoEqual, Value::Nil(_)) => todo!(),
        (Value::Nil(_), BinaryOp::TwoEqual, _) => v_false,

        // Pointer == _
        (Value::Pointer(_), BinaryOp::TwoEqual, Value::Pointer(_)) => todo!(),
        (Value::Pointer(_), BinaryOp::TwoEqual, _) => v_false,

        // Number _ Number
        (Value::Number(_), _, Value::Number(_)) => todo!(),

        // Number _ NumberInterval
        // TODO

        // NumberInterval _ Number
        // TODO

        // NumberInterval _ NumberInterval
        // TODO

        // _ .. _
        (Value::String(_), BinaryOp::TwoDots, Value::String(_)) => todo!(),
        (Value::String(_), BinaryOp::TwoDots, Value::Number(_)) => todo!(),
        (Value::Number(_), BinaryOp::TwoDots, Value::String(_)) => todo!(),

        _ => panic!("Unsupported binary op: {:?} {:?} {:?}", l, op, r),
    }
}
