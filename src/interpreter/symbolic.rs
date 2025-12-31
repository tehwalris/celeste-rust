//! Symbolic tracing for vectorized execution.
//!
//! This module implements symbolic execution with counting DFS for path enumeration.
//! Values are traced as (concrete, symbol) pairs, allowing us to replay traced
//! transformations onto compatible vector elements.

use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::pico8_num::{Pico8Num, Pico8NumInterval};

use super::heap::HeapId;

/// A symbolic identifier for an input value.
/// Created at frame start, referenced in symbolic expressions.
#[derive(Clone, Copy, Debug, Hash, Eq, PartialEq, Serialize, Deserialize)]
pub struct SymbolId(pub u32);

/// Concrete scalar values that can appear in symbolic expressions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ConcreteValue {
    Number(Pico8Num),
    NumberInterval(Pico8NumInterval),
    Bool(bool),
    String(Arc<String>),
    Nil,
    Pointer(HeapId),
}

/// A symbolic expression representing how a value was computed.
/// Uses Arc for efficient cloning of shared subexpressions.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SymExpr {
    /// Input symbol (created at frame start)
    Input(SymbolId),

    /// Constant value
    Const(ConcreteValue),

    // Arithmetic operations
    Add(Arc<SymExpr>, Arc<SymExpr>),
    Sub(Arc<SymExpr>, Arc<SymExpr>),
    Mul(Arc<SymExpr>, Arc<SymExpr>),
    Div(Arc<SymExpr>, Arc<SymExpr>),
    Mod(Arc<SymExpr>, Arc<SymExpr>),
    Neg(Arc<SymExpr>),
    Flr(Arc<SymExpr>),

    // Comparison operations (produce bool)
    Lt(Arc<SymExpr>, Arc<SymExpr>),
    Le(Arc<SymExpr>, Arc<SymExpr>),
    Gt(Arc<SymExpr>, Arc<SymExpr>),
    Ge(Arc<SymExpr>, Arc<SymExpr>),
    Eq(Arc<SymExpr>, Arc<SymExpr>),
    Ne(Arc<SymExpr>, Arc<SymExpr>),

    // Boolean operations
    And(Arc<SymExpr>, Arc<SymExpr>),
    Or(Arc<SymExpr>, Arc<SymExpr>),
    Not(Arc<SymExpr>),

    // Bitwise operations
    Band(Arc<SymExpr>, Arc<SymExpr>),
    Bor(Arc<SymExpr>, Arc<SymExpr>),
    Bxor(Arc<SymExpr>, Arc<SymExpr>),
    Bnot(Arc<SymExpr>),
    Shl(Arc<SymExpr>, Arc<SymExpr>),
    Shr(Arc<SymExpr>, Arc<SymExpr>),
    Lshr(Arc<SymExpr>, Arc<SymExpr>),
    Rotl(Arc<SymExpr>, Arc<SymExpr>),
    Rotr(Arc<SymExpr>, Arc<SymExpr>),

    // Math functions
    Abs(Arc<SymExpr>),
    Sgn(Arc<SymExpr>),
    Min(Arc<SymExpr>, Arc<SymExpr>),
    Max(Arc<SymExpr>, Arc<SymExpr>),
    Mid(Arc<SymExpr>, Arc<SymExpr>, Arc<SymExpr>),
    Sin(Arc<SymExpr>),
    Cos(Arc<SymExpr>),
    Atan2(Arc<SymExpr>, Arc<SymExpr>),
    Sqrt(Arc<SymExpr>),
    Rnd(Arc<SymExpr>),

    // String operations
    Sub8(Arc<SymExpr>, Arc<SymExpr>, Arc<SymExpr>),
    Concat(Arc<SymExpr>, Arc<SymExpr>),

    // Type conversions
    Tonum(Arc<SymExpr>),
    Tostr(Arc<SymExpr>),
    Chr(Arc<SymExpr>),
    Ord(Arc<SymExpr>),

    // Conditional (ternary)
    IfThenElse(Arc<SymExpr>, Arc<SymExpr>, Arc<SymExpr>),

    // Interval operations
    IntervalLow(Arc<SymExpr>),
    IntervalHigh(Arc<SymExpr>),
    MakeInterval(Arc<SymExpr>, Arc<SymExpr>),
}

impl SymExpr {
    /// Create a constant number expression
    pub fn num(n: Pico8Num) -> Self {
        SymExpr::Const(ConcreteValue::Number(n))
    }

    /// Create a constant bool expression
    pub fn bool(b: bool) -> Self {
        SymExpr::Const(ConcreteValue::Bool(b))
    }

    /// Create a constant nil expression
    pub fn nil() -> Self {
        SymExpr::Const(ConcreteValue::Nil)
    }

    /// Create a constant string expression
    pub fn string(s: String) -> Self {
        SymExpr::Const(ConcreteValue::String(Arc::new(s)))
    }

    /// Create a constant pointer expression
    pub fn pointer(id: HeapId) -> Self {
        SymExpr::Const(ConcreteValue::Pointer(id))
    }

    /// Create an input symbol expression
    pub fn input(id: SymbolId) -> Self {
        SymExpr::Input(id)
    }

    /// Binary operation helper
    fn binop(op: fn(Arc<SymExpr>, Arc<SymExpr>) -> SymExpr, a: SymExpr, b: SymExpr) -> SymExpr {
        op(Arc::new(a), Arc::new(b))
    }

    /// Unary operation helper
    fn unop(op: fn(Arc<SymExpr>) -> SymExpr, a: SymExpr) -> SymExpr {
        op(Arc::new(a))
    }

    // Convenience constructors for common operations
    pub fn add(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Add, a, b)
    }
    pub fn sub(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Sub, a, b)
    }
    pub fn mul(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Mul, a, b)
    }
    pub fn div(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Div, a, b)
    }
    pub fn lt(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Lt, a, b)
    }
    pub fn le(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Le, a, b)
    }
    pub fn gt(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Gt, a, b)
    }
    pub fn ge(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Ge, a, b)
    }
    pub fn eq(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Eq, a, b)
    }
    pub fn ne(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Ne, a, b)
    }
    pub fn and(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::And, a, b)
    }
    pub fn or(a: SymExpr, b: SymExpr) -> SymExpr {
        Self::binop(SymExpr::Or, a, b)
    }
    pub fn not(a: SymExpr) -> SymExpr {
        Self::unop(SymExpr::Not, a)
    }
    pub fn neg(a: SymExpr) -> SymExpr {
        Self::unop(SymExpr::Neg, a)
    }
}

/// A traced value: concrete result + symbolic representation.
#[derive(Clone, Debug)]
pub struct TracedValue {
    /// The concrete value (used for path decisions during tracing)
    pub concrete: ConcreteValue,
    /// How to compute this value from inputs
    pub symbol: SymExpr,
}

impl TracedValue {
    /// Create a traced value from a constant
    pub fn constant(value: ConcreteValue) -> Self {
        Self {
            symbol: SymExpr::Const(value.clone()),
            concrete: value,
        }
    }

    /// Create a traced value with a fresh input symbol
    pub fn input(id: SymbolId, concrete: ConcreteValue) -> Self {
        Self {
            concrete,
            symbol: SymExpr::Input(id),
        }
    }
}


/// Generator for fresh symbol IDs.
#[derive(Clone, Debug, Default)]
pub struct SymbolGenerator {
    next_id: u32,
}

impl SymbolGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn fresh(&mut self) -> SymbolId {
        let id = SymbolId(self.next_id);
        self.next_id += 1;
        id
    }
}

/// Substitution map for evaluating symbolic expressions.
pub type Substitution = std::collections::HashMap<SymbolId, ConcreteValue>;

/// Evaluate a symbolic expression with a substitution map.
/// Returns the concrete value resulting from the evaluation.
pub fn evaluate_sym_expr(expr: &SymExpr, subst: &Substitution) -> ConcreteValue {
    match expr {
        SymExpr::Input(sym_id) => {
            subst.get(sym_id).cloned().unwrap_or(ConcreteValue::Nil)
        }
        SymExpr::Const(value) => value.clone(),

        // Arithmetic operations
        SymExpr::Add(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_add(l, r)
        }
        SymExpr::Sub(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_sub(l, r)
        }
        SymExpr::Mul(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_mul(l, r)
        }
        SymExpr::Div(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_div(l, r)
        }
        SymExpr::Mod(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_mod(l, r)
        }
        SymExpr::Neg(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_neg(a)
        }
        SymExpr::Flr(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_flr(a)
        }

        // Comparison operations
        SymExpr::Lt(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_lt(l, r)
        }
        SymExpr::Le(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_le(l, r)
        }
        SymExpr::Gt(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_gt(l, r)
        }
        SymExpr::Ge(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_ge(l, r)
        }
        SymExpr::Eq(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_eq(l, r)
        }
        SymExpr::Ne(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_ne(l, r)
        }

        // Boolean operations
        SymExpr::And(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_and(l, r)
        }
        SymExpr::Or(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_or(l, r)
        }
        SymExpr::Not(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_not(a)
        }

        // Bitwise operations
        SymExpr::Band(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_band(l, r)
        }
        SymExpr::Bor(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_bor(l, r)
        }
        SymExpr::Bxor(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_bxor(l, r)
        }
        SymExpr::Bnot(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_bnot(a)
        }
        SymExpr::Shl(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_shl(l, r)
        }
        SymExpr::Shr(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_shr(l, r)
        }
        SymExpr::Lshr(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_lshr(l, r)
        }
        SymExpr::Rotl(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_rotl(l, r)
        }
        SymExpr::Rotr(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_rotr(l, r)
        }

        // Math functions
        SymExpr::Abs(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_abs(a)
        }
        SymExpr::Sgn(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_sgn(a)
        }
        SymExpr::Min(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_min(l, r)
        }
        SymExpr::Max(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_max(l, r)
        }
        SymExpr::Mid(a, b, c) => {
            let av = evaluate_sym_expr(a, subst);
            let bv = evaluate_sym_expr(b, subst);
            let cv = evaluate_sym_expr(c, subst);
            eval_mid(av, bv, cv)
        }
        SymExpr::Sin(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_sin(a)
        }
        SymExpr::Cos(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_cos(a)
        }
        SymExpr::Atan2(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_atan2(l, r)
        }
        SymExpr::Sqrt(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_sqrt(a)
        }
        SymExpr::Rnd(arg) => {
            // Random - just return the argument for deterministic behavior
            evaluate_sym_expr(arg, subst)
        }

        // String operations
        SymExpr::Sub8(s, start, len) => {
            let sv = evaluate_sym_expr(s, subst);
            let startv = evaluate_sym_expr(start, subst);
            let lenv = evaluate_sym_expr(len, subst);
            eval_sub8(sv, startv, lenv)
        }
        SymExpr::Concat(left, right) => {
            let l = evaluate_sym_expr(left, subst);
            let r = evaluate_sym_expr(right, subst);
            eval_concat(l, r)
        }

        // Type conversions
        SymExpr::Tonum(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_tonum(a)
        }
        SymExpr::Tostr(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_tostr(a)
        }
        SymExpr::Chr(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_chr(a)
        }
        SymExpr::Ord(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_ord(a)
        }

        // Conditional
        SymExpr::IfThenElse(cond, then_expr, else_expr) => {
            let c = evaluate_sym_expr(cond, subst);
            if is_truthy(&c) {
                evaluate_sym_expr(then_expr, subst)
            } else {
                evaluate_sym_expr(else_expr, subst)
            }
        }

        // Interval operations
        SymExpr::IntervalLow(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_interval_low(a)
        }
        SymExpr::IntervalHigh(arg) => {
            let a = evaluate_sym_expr(arg, subst);
            eval_interval_high(a)
        }
        SymExpr::MakeInterval(low, high) => {
            let l = evaluate_sym_expr(low, subst);
            let h = evaluate_sym_expr(high, subst);
            eval_make_interval(l, h)
        }
    }
}

// Helper evaluation functions
fn is_truthy(v: &ConcreteValue) -> bool {
    match v {
        ConcreteValue::Nil => false,
        ConcreteValue::Bool(b) => *b,
        _ => true,
    }
}

fn get_num(v: &ConcreteValue) -> Option<Pico8Num> {
    match v {
        ConcreteValue::Number(n) => Some(*n),
        _ => None,
    }
}

fn eval_add(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a + b),
        _ => ConcreteValue::Nil,
    }
}

fn eval_sub(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a - b),
        _ => ConcreteValue::Nil,
    }
}

fn eval_mul(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a * b),
        _ => ConcreteValue::Nil,
    }
}

fn eval_div(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a / b),
        _ => ConcreteValue::Nil,
    }
}

fn eval_mod(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a % b),
        _ => ConcreteValue::Nil,
    }
}

fn eval_neg(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => ConcreteValue::Number(-n),
        _ => ConcreteValue::Nil,
    }
}

fn eval_flr(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => ConcreteValue::Number(n.flr()),
        _ => ConcreteValue::Nil,
    }
}

fn eval_lt(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Bool(a < b),
        _ => ConcreteValue::Bool(false),
    }
}

fn eval_le(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Bool(a <= b),
        _ => ConcreteValue::Bool(false),
    }
}

fn eval_gt(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Bool(a > b),
        _ => ConcreteValue::Bool(false),
    }
}

fn eval_ge(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Bool(a >= b),
        _ => ConcreteValue::Bool(false),
    }
}

fn eval_eq(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    ConcreteValue::Bool(l == r)
}

fn eval_ne(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    ConcreteValue::Bool(l != r)
}

fn eval_and(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    if is_truthy(&l) { r } else { l }
}

fn eval_or(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    if is_truthy(&l) { l } else { r }
}

fn eval_not(a: ConcreteValue) -> ConcreteValue {
    ConcreteValue::Bool(!is_truthy(&a))
}

fn eval_band(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            // Bitwise AND on raw i32 values
            ConcreteValue::Number(Pico8Num::from_raw(a.as_raw_i32() & b.as_raw_i32()))
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_bor(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            ConcreteValue::Number(Pico8Num::from_raw(a.as_raw_i32() | b.as_raw_i32()))
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_bxor(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            ConcreteValue::Number(Pico8Num::from_raw(a.as_raw_i32() ^ b.as_raw_i32()))
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_bnot(a: ConcreteValue) -> ConcreteValue {
    match &a {
        ConcreteValue::Number(n) => {
            ConcreteValue::Number(Pico8Num::from_raw(!n.as_raw_i32()))
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_shl(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            if let Some(shift) = b.as_i16() {
                let shift = shift.clamp(0, 31) as u32;
                ConcreteValue::Number(Pico8Num::from_raw(a.as_raw_i32() << shift))
            } else {
                ConcreteValue::Nil
            }
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_shr(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            if let Some(shift) = b.as_i16() {
                let shift = shift.clamp(0, 31) as u32;
                ConcreteValue::Number(Pico8Num::from_raw(a.as_raw_i32() >> shift))
            } else {
                ConcreteValue::Nil
            }
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_lshr(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            if let Some(shift) = b.as_i16() {
                let shift = shift.clamp(0, 31) as u32;
                ConcreteValue::Number(Pico8Num::from_raw((a.as_raw_u32() >> shift) as i32))
            } else {
                ConcreteValue::Nil
            }
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_rotl(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            if let Some(shift) = b.as_i16() {
                let shift = (shift as u32) % 32;
                let v = a.as_raw_u32();
                ConcreteValue::Number(Pico8Num::from_raw((v.rotate_left(shift)) as i32))
            } else {
                ConcreteValue::Nil
            }
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_rotr(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (&l, &r) {
        (ConcreteValue::Number(a), ConcreteValue::Number(b)) => {
            if let Some(shift) = b.as_i16() {
                let shift = (shift as u32) % 32;
                let v = a.as_raw_u32();
                ConcreteValue::Number(Pico8Num::from_raw((v.rotate_right(shift)) as i32))
            } else {
                ConcreteValue::Nil
            }
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_abs(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => ConcreteValue::Number(n.abs()),
        _ => ConcreteValue::Nil,
    }
}

fn eval_sgn(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => ConcreteValue::Number(n.sgn()),
        _ => ConcreteValue::Nil,
    }
}

fn eval_min(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a.min(b)),
        _ => ConcreteValue::Nil,
    }
}

fn eval_max(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a.max(b)),
        _ => ConcreteValue::Nil,
    }
}

fn eval_mid(a: ConcreteValue, b: ConcreteValue, c: ConcreteValue) -> ConcreteValue {
    match (get_num(&a), get_num(&b), get_num(&c)) {
        (Some(x), Some(y), Some(z)) => ConcreteValue::Number(x.mid(y, z)),
        _ => ConcreteValue::Nil,
    }
}

fn eval_sin(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => ConcreteValue::Number(n.sin()),
        _ => ConcreteValue::Nil,
    }
}

fn eval_cos(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => ConcreteValue::Number(n.cos()),
        _ => ConcreteValue::Nil,
    }
}

fn eval_atan2(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&r)) {
        (Some(a), Some(b)) => ConcreteValue::Number(a.atan2(b)),
        _ => ConcreteValue::Nil,
    }
}

fn eval_sqrt(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => ConcreteValue::Number(n.sqrt()),
        _ => ConcreteValue::Nil,
    }
}

fn eval_sub8(_s: ConcreteValue, _start: ConcreteValue, _len: ConcreteValue) -> ConcreteValue {
    // String substring - simplified for now
    ConcreteValue::String(Arc::new(String::new()))
}

fn eval_concat(l: ConcreteValue, r: ConcreteValue) -> ConcreteValue {
    match (l, r) {
        (ConcreteValue::String(a), ConcreteValue::String(b)) => {
            ConcreteValue::String(Arc::new(format!("{}{}", a, b)))
        }
        _ => ConcreteValue::String(Arc::new(String::new())),
    }
}

fn eval_tonum(a: ConcreteValue) -> ConcreteValue {
    match a {
        ConcreteValue::Number(n) => ConcreteValue::Number(n),
        ConcreteValue::String(s) => {
            if let Ok(n) = s.parse::<f64>() {
                ConcreteValue::Number(Pico8Num::from_f64(n))
            } else {
                ConcreteValue::Nil
            }
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_tostr(a: ConcreteValue) -> ConcreteValue {
    match a {
        ConcreteValue::Number(n) => ConcreteValue::String(Arc::new(format!("{}", n.to_f64()))),
        ConcreteValue::String(s) => ConcreteValue::String(s),
        ConcreteValue::Bool(b) => ConcreteValue::String(Arc::new(if b { "true" } else { "false" }.to_string())),
        ConcreteValue::Nil => ConcreteValue::String(Arc::new("nil".to_string())),
        _ => ConcreteValue::String(Arc::new(String::new())),
    }
}

fn eval_chr(a: ConcreteValue) -> ConcreteValue {
    match get_num(&a) {
        Some(n) => {
            if let Some(i) = n.as_i16() {
                if i >= 0 && i <= 127 {
                    ConcreteValue::String(Arc::new((i as u8 as char).to_string()))
                } else {
                    ConcreteValue::String(Arc::new(String::new()))
                }
            } else {
                ConcreteValue::String(Arc::new(String::new()))
            }
        }
        _ => ConcreteValue::String(Arc::new(String::new())),
    }
}

fn eval_ord(a: ConcreteValue) -> ConcreteValue {
    match a {
        ConcreteValue::String(s) => {
            if let Some(c) = s.chars().next() {
                ConcreteValue::Number(Pico8Num::from_i16(c as i16))
            } else {
                ConcreteValue::Nil
            }
        }
        _ => ConcreteValue::Nil,
    }
}

fn eval_interval_low(a: ConcreteValue) -> ConcreteValue {
    match a {
        ConcreteValue::NumberInterval(interval) => ConcreteValue::Number(interval.low),
        ConcreteValue::Number(n) => ConcreteValue::Number(n),
        _ => ConcreteValue::Nil,
    }
}

fn eval_interval_high(a: ConcreteValue) -> ConcreteValue {
    match a {
        ConcreteValue::NumberInterval(interval) => ConcreteValue::Number(interval.high),
        ConcreteValue::Number(n) => ConcreteValue::Number(n),
        _ => ConcreteValue::Nil,
    }
}

fn eval_make_interval(l: ConcreteValue, h: ConcreteValue) -> ConcreteValue {
    match (get_num(&l), get_num(&h)) {
        (Some(low), Some(high)) => ConcreteValue::NumberInterval(Pico8NumInterval { low, high }),
        _ => ConcreteValue::Nil,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_symbol_generator() {
        let mut gen = SymbolGenerator::new();
        assert_eq!(gen.fresh(), SymbolId(0));
        assert_eq!(gen.fresh(), SymbolId(1));
        assert_eq!(gen.fresh(), SymbolId(2));
    }

    #[test]
    fn test_sym_expr_construction() {
        let a = SymExpr::input(SymbolId(0));
        let b = SymExpr::input(SymbolId(1));
        let sum = SymExpr::add(a, b);

        match sum {
            SymExpr::Add(left, right) => {
                assert!(matches!(*left, SymExpr::Input(SymbolId(0))));
                assert!(matches!(*right, SymExpr::Input(SymbolId(1))));
            }
            _ => panic!("Expected Add"),
        }
    }

    #[test]
    fn test_traced_value_constant() {
        let tv = TracedValue::constant(ConcreteValue::Bool(true));
        assert_eq!(tv.concrete, ConcreteValue::Bool(true));
        assert!(matches!(tv.symbol, SymExpr::Const(ConcreteValue::Bool(true))));
    }

    #[test]
    fn test_traced_value_input() {
        let tv = TracedValue::input(SymbolId(42), ConcreteValue::Number(Pico8Num::from_i16(10)));
        assert_eq!(tv.concrete, ConcreteValue::Number(Pico8Num::from_i16(10)));
        assert!(matches!(tv.symbol, SymExpr::Input(SymbolId(42))));
    }
}
