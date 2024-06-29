use std::collections::HashMap;

use itertools::Itertools;

use super::heap::HeapId;
use crate::{ir::GlobalId, pico8_num::Pico8Num};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MaybeVector<T: std::fmt::Debug + Clone + PartialEq + Eq> {
    Scalar(T),
    // TODO do we want a link to some kind of size provider?
    Vector(Vec<T>),
}

impl<T: std::fmt::Debug + Clone + PartialEq + Eq> MaybeVector<T> {
    pub fn map(&self, f: impl Fn(&T) -> T) -> Self {
        match self {
            MaybeVector::Scalar(v) => MaybeVector::Scalar(f(v)),
            MaybeVector::Vector(v) => MaybeVector::Vector(v.iter().map(f).collect()),
        }
    }

    pub fn map2<O: std::fmt::Debug + Clone + PartialEq + Eq>(
        a: &Self,
        b: &Self,
        f: impl Fn(&T, &T) -> O,
    ) -> MaybeVector<O> {
        match (a, b) {
            (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => MaybeVector::Scalar(f(a, b)),
            (MaybeVector::Vector(a), MaybeVector::Vector(b)) => {
                MaybeVector::Vector(a.iter().zip_eq(b.iter()).map(|(a, b)| f(a, b)).collect())
            }
            _ => panic!("Mismatched vector sizes"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Value {
    Number(MaybeVector<Pico8Num>),
    // NumberInterval(MaybeVector<PicoNumberInterval>), // TODO
    Bool(MaybeVector<bool>),
    UnknownBool,
    String(String),
    Nil(Option<String>),
    Pointer(HeapId),
    NilPointer(String),
}

fn filter_vec_by_mask<T>(vec: Vec<T>, mask: &[bool]) -> Vec<T> {
    vec.into_iter()
        .zip(mask.iter())
        .filter_map(|(v, m)| if *m { Some(v) } else { None })
        .collect()
}

impl Value {
    pub fn filter_vectors(self, mask: &[bool]) -> Self {
        match self {
            Value::Bool(MaybeVector::Vector(vec)) => {
                Value::Bool(MaybeVector::Vector(filter_vec_by_mask(vec, mask)))
            }
            Value::Number(MaybeVector::Vector(vec)) => {
                Value::Number(MaybeVector::Vector(filter_vec_by_mask(vec, mask)))
            }
            _ => self,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum HeapValue {
    Value(Value),
    ObjectTable(HashMap<String, HeapId>),
    ArrayTable(Vec<HeapId>),
    UnknownTable,
    Closure(GlobalId, Vec<Value>),
    BuiltinFun(String),
}
