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
