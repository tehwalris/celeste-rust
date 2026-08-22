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
        v => Err(anyhow!("Unsupported value for not: {:?}", v)),
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

/// Per-lane handling of MIXED interval comparisons, where some lanes have
/// a definite answer and some straddle. ON by default; set
/// CELESTE_NO_PARTITION_STRADDLES to opt out (for A/B measurement only).
///
/// Building a whole-value `UnknownBool` from such a comparison is a join
/// ACROSS LANES - one lane's ambiguity destroying its neighbours' answers.
/// That is a CORRECTNESS property, not an optimization: a lane's result
/// must not depend on which other lanes happen to share its state, or
/// batching stops being a pure implementation detail and starts changing
/// what the search computes. It is also why the collapse is chunk-size
/// dependent, since whether it fires depends on whether ANY lane in the
/// state straddles. `simdcheck` is the acceptance gate for exactly this.
///
/// So the comparison yields a `MaybeBool` transient that
/// `core_interpreter::partition_maybe_bool` resolves by SPLITTING THE
/// STATE: definite lanes keep a real `Bool`, straddling lanes leave in a
/// spill state whose `UnknownBool` is then honest.
///
/// The first design duplicated the ambiguous lanes instead of moving
/// them, and OOMed at 102 GB inside room (0,0) frame 66 - `obj.collide`
/// has several mixed comparisons in a row with no merge between them, so
/// the copies multiplied. A partition moves lanes and never creates one,
/// which fixed that: measured at f70 the fragment count moves 0.05%.
///
/// It is on by default because lane independence is wanted whether or not
/// it pays, and it very nearly does not: at f70 it costs 4.6% peak RSS,
/// is neutral on time, and cuts the deopt only 1.4%. That is because it
/// targets mixed comparisons while 83% of collapses at depth are
/// ALL-unknown - where the whole-value tag is honest and no split is
/// possible. Those are handled by splitting `select` instead.
/// 0 = not yet read, 1 = off, 2 = on. An atomic rather than a OnceLock so
/// tests can drive both paths; the env var is the default, read once.
static PARTITION_STRADDLES: std::sync::atomic::AtomicU8 = std::sync::atomic::AtomicU8::new(0);

fn partition_straddles_enabled() -> bool {
    use std::sync::atomic::Ordering;
    match PARTITION_STRADDLES.load(Ordering::Relaxed) {
        0 => {
            let on = std::env::var_os("CELESTE_NO_PARTITION_STRADDLES").is_none();
            PARTITION_STRADDLES.store(if on { 2 } else { 1 }, Ordering::Relaxed);
            on
        }
        1 => false,
        _ => true,
    }
}

/// Force the partition path on or off, overriding the environment.
/// For tests, which must be able to exercise both paths regardless of how
/// the suite was invoked.
///
/// Not `#[cfg(test)]`, and neither is the lock below: the tests that use
/// them live in `celeste-rust`, and `cfg(test)` does not cross a crate
/// boundary - it would configure these out of the dependency exactly when
/// the dependent's tests want them.
pub fn set_partition_straddles(on: bool) {
    PARTITION_STRADDLES.store(if on { 2 } else { 1 }, std::sync::atomic::Ordering::Relaxed);
}

/// Lock for tests whose result depends on `PARTITION_STRADDLES` - the
/// tests that DRIVE it and the differential tests that assume its
/// default, both. Under `cargo test`'s shared process a toggle test
/// flipping the flag mid-way through another test's baseline/candidate
/// pair makes the pair diverge (observed: variant dispatch "diverged at
/// frame 24" - the baseline ran with partitioning off, the candidate
/// with it on). Under nextest this lock is uncontended, so it costs
/// nothing where the suite actually runs.
pub fn partition_straddles_test_lock() -> std::sync::MutexGuard<'static, ()> {
    static LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());
    LOCK.lock().unwrap_or_else(|e| e.into_inner())
}

/// Helper to lift a number to an interval
fn lift_to_interval(v: &MaybeVector<Pico8Num>) -> MaybeVector<Pico8NumInterval> {
    v.map_to(|n| Pico8NumInterval::from_number(*n))
}

/// Per-lane truthiness, following the same rules as a conditional branch
/// (`flow.rs`): only `false` and `nil` are false, everything else is true.
///
/// `None` means the condition cannot be resolved per lane. That is
/// `UnknownBool`, which a branch handles by sending the state down *both* edges
/// unfiltered - there is no lane-wise answer to give, so a `select` on it has
/// to fail.
fn lane_condition(v: &Value) -> Option<MaybeVector<bool>> {
    match v {
        Value::Bool(b) => Some(b.clone()),
        Value::Nil(_) => Some(MaybeVector::Scalar(false)),
        Value::Number(_)
        | Value::NumberInterval(_)
        | Value::String(_)
        | Value::Pointer(_) => Some(MaybeVector::Scalar(true)),
        Value::UnknownBool | Value::NilPointer(_) => None,
        // Transient (see value.rs): it is resolved into a definite `Bool`
        // before assignment, so a select should never receive one. Reported
        // as "no per-lane value" rather than panicking, so the deopt path
        // can still absorb it if some route ever does reach here.
        Value::MaybeBool(_) => None,
    }
}

/// Pick `if_true` or `if_false` per lane.
///
/// Deliberately partial - see `Instruction::Select`. A value carries one type
/// tag and one `HeapId` for all its lanes, so this can only combine two values
/// of the same numeric-ish representation, or two values that are already
/// equal. Anything else is an error rather than a widening, because the whole
/// reason to run a `select` is to avoid splitting the state, and silently
/// losing per-lane information would defeat the purpose in the worst possible
/// way: quietly.
pub fn interpret_select(condition: &Value, if_true: &Value, if_false: &Value) -> Result<Value> {
    let Some(mask) = lane_condition(condition) else {
        return Err(anyhow!(
            "select on a condition with no per-lane value: {:?}",
            condition
        ));
    };

    // A uniform condition needs no combining, and this is the case that lets a
    // select survive on values it could not otherwise merge - a pointer, say.
    // The census counts what the *discarded* arm cost upstream: a vector arm
    // was per-lane compute this point throws away (class-dead speculation).
    let discarded_lanes = |v: &Value| -> usize {
        match v {
            Value::Number(MaybeVector::Vector(x)) => x.len(),
            Value::NumberInterval(MaybeVector::Vector(x)) => x.len(),
            Value::Bool(MaybeVector::Vector(x)) => x.len(),
            _ => 0,
        }
    };
    match &mask {
        MaybeVector::Scalar(true) => {
            if crate::op_census::enabled() {
                crate::op_census::record_select_uniform(discarded_lanes(if_false));
            }
            return Ok(if_true.clone());
        }
        MaybeVector::Scalar(false) => {
            if crate::op_census::enabled() {
                crate::op_census::record_select_uniform(discarded_lanes(if_true));
            }
            return Ok(if_false.clone());
        }
        MaybeVector::Vector(m) if m.iter().all(|b| *b) => {
            if crate::op_census::enabled() {
                crate::op_census::record_select_uniform(discarded_lanes(if_false));
            }
            return Ok(if_true.clone());
        }
        MaybeVector::Vector(m) if m.iter().all(|b| !*b) => {
            if crate::op_census::enabled() {
                crate::op_census::record_select_uniform(discarded_lanes(if_true));
            }
            return Ok(if_false.clone());
        }
        MaybeVector::Vector(_) => {
            if crate::op_census::enabled() {
                crate::op_census::record_select_mixed();
            }
        }
    }

    if if_true == if_false {
        return Ok(if_true.clone());
    }
    // A mixed-mask select that actually combines lanes: price the memo.
    memo_price(0x5e1e_c7_5e1e_c7u64, &[condition, if_true, if_false]);

    fn pick<T: std::fmt::Debug + Clone + PartialEq + Eq>(
        mask: &MaybeVector<bool>,
        a: &MaybeVector<T>,
        b: &MaybeVector<T>,
    ) -> MaybeVector<T> {
        let MaybeVector::Vector(mask) = mask else {
            unreachable!("uniform conditions are handled above")
        };
        let t = crate::op_census::start();
        // Specialized per arm representation: select is the largest flat
        // instruction cost at depth (13.4 s at frame 44), and the generic
        // loop paid a per-lane match on each arm. Each specialization is a
        // branchless-friendly loop the compiler can vectorize; the
        // scalar/scalar case in particular is a mask-to-value map with no
        // per-lane indexing at all.
        let out: Vec<T> = match (a, b) {
            (MaybeVector::Scalar(a), MaybeVector::Scalar(b)) => mask
                .iter()
                .map(|&take_true| if take_true { a.clone() } else { b.clone() })
                .collect(),
            (MaybeVector::Scalar(a), MaybeVector::Vector(b)) => mask
                .iter()
                .zip(b.iter())
                .map(|(&take_true, b)| if take_true { a.clone() } else { b.clone() })
                .collect(),
            (MaybeVector::Vector(a), MaybeVector::Scalar(b)) => mask
                .iter()
                .zip(a.iter())
                .map(|(&take_true, a)| if take_true { a.clone() } else { b.clone() })
                .collect(),
            (MaybeVector::Vector(a), MaybeVector::Vector(b)) => mask
                .iter()
                .zip(a.iter().zip(b.iter()))
                .map(|(&take_true, (a, b))| if take_true { a.clone() } else { b.clone() })
                .collect(),
        };
        crate::op_census::record(
            crate::op_census::Cat::Select,
            mask.len(),
            mask.len() * (1 + 3 * std::mem::size_of::<T>()),
            t,
        );
        MaybeVector::vector(out)
    }

    /// A boolean arm as one entry per lane, broadcasting the scalar form.
    fn bool_lanes(v: &MaybeVector<bool>, lanes: usize) -> Vec<bool> {
        match v {
            MaybeVector::Scalar(b) => vec![*b; lanes],
            MaybeVector::Vector(v) => v.as_ref().clone(),
        }
    }

    match (if_true, if_false) {
        (Value::Number(a), Value::Number(b)) => Ok(Value::Number(pick(&mask, a, b))),
        (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(pick(&mask, a, b))),
        (Value::NumberInterval(a), Value::NumberInterval(b)) => {
            Ok(Value::NumberInterval(pick(&mask, a, b)))
        }
        // Mixing a number with an interval is fine; that is the same widening
        // the binary operators already do.
        (Value::Number(a), Value::NumberInterval(b)) => Ok(Value::NumberInterval(pick(
            &mask,
            &lift_to_interval(a),
            b,
        ))),
        (Value::NumberInterval(a), Value::Number(b)) => Ok(Value::NumberInterval(pick(
            &mask,
            a,
            &lift_to_interval(b),
        ))),
        // Both arms unknown: so is every lane of the result, and the
        // whole-value tag is honest. No gate needed - this loses nothing.
        (Value::UnknownBool, Value::UnknownBool) => Ok(Value::UnknownBool),
        // ONE boolean arm is a whole-value `UnknownBool`. The lanes the mask
        // takes from the DEFINITE arm still have an answer, so collapsing
        // the result to `UnknownBool` - or failing, which is what this used
        // to do - is again a join across lanes.
        //
        // This is the second link of the strawberry's `and` chain. Splitting
        // the CONDITION (see `split_select_on_unknown`) turns `%157` into a
        // real per-lane `Bool`, and then `%189 = select %157 ? %186 : %157`
        // fails here instead, with `%186` unknown and `%157` mixed. Mixed is
        // the point: there are definite lanes to keep.
        (Value::UnknownBool, Value::Bool(b)) if partition_straddles_enabled() => {
            let MaybeVector::Vector(m) = &mask else {
                unreachable!("uniform conditions are handled above")
            };
            let b = bool_lanes(b, m.len());
            Ok(Value::MaybeBool(MaybeVector::vector(
                m.iter()
                    .zip(b)
                    .map(|(&take_true, b)| if take_true { None } else { Some(b) })
                    .collect(),
            )))
        }
        (Value::Bool(a), Value::UnknownBool) if partition_straddles_enabled() => {
            let MaybeVector::Vector(m) = &mask else {
                unreachable!("uniform conditions are handled above")
            };
            let a = bool_lanes(a, m.len());
            Ok(Value::MaybeBool(MaybeVector::vector(
                m.iter()
                    .zip(a)
                    .map(|(&take_true, a)| if take_true { Some(a) } else { None })
                    .collect(),
            )))
        }
        _ => Err(anyhow!(
            "select cannot combine {:?} and {:?} per lane - a value carries one \
             type tag and one HeapId for all its lanes",
            if_true,
            if_false
        )),
    }
}
/// The vector Arc inside a value, as (address, type-erased strong ref).
fn vec_arc_of(v: &Value) -> Option<(usize, std::sync::Arc<dyn std::any::Any + Send + Sync>)> {
    match v {
        Value::Number(MaybeVector::Vector(a)) => {
            Some((std::sync::Arc::as_ptr(a) as usize, a.clone()))
        }
        Value::NumberInterval(MaybeVector::Vector(a)) => {
            Some((std::sync::Arc::as_ptr(a) as usize, a.clone()))
        }
        Value::Bool(MaybeVector::Vector(a)) => {
            Some((std::sync::Arc::as_ptr(a) as usize, a.clone()))
        }
        _ => None,
    }
}

fn value_lanes(v: &Value) -> usize {
    match v {
        Value::Number(MaybeVector::Vector(a)) => a.len(),
        Value::NumberInterval(MaybeVector::Vector(a)) => a.len(),
        Value::Bool(MaybeVector::Vector(a)) => a.len(),
        _ => 1,
    }
}

/// Price the cross-fragment op memo (census only): would this vector op's
/// inputs have been seen before, Arc-identically, this frame?
fn memo_price(tag: u64, inputs: &[&Value]) {
    if !crate::op_census::enabled() {
        return;
    }
    use std::hash::{Hash, Hasher};
    let mut hasher = rustc_hash::FxHasher::default();
    tag.hash(&mut hasher);
    let mut holders = Vec::new();
    let mut elems = 1usize;
    let mut any_vector = false;
    for value in inputs {
        match vec_arc_of(value) {
            Some((ptr, holder)) => {
                any_vector = true;
                ptr.hash(&mut hasher);
                elems = elems.max(value_lanes(value));
                holders.push(holder);
            }
            None => {
                // A scalar operand's identity is its value: hash the whole
                // value so ops differing only in a scalar operand do not
                // fake a memo hit.
                match value {
                    Value::Number(MaybeVector::Scalar(n)) => (1u8, n).hash(&mut hasher),
                    Value::NumberInterval(MaybeVector::Scalar(iv)) => {
                        (2u8, iv.low, iv.high).hash(&mut hasher)
                    }
                    Value::Bool(MaybeVector::Scalar(b)) => (3u8, b).hash(&mut hasher),
                    other => (4u8, format!("{other:?}")).hash(&mut hasher),
                }
            }
        }
    }
    if any_vector {
        crate::op_census::memo_probe(hasher.finish(), elems, holders);
    }
}

pub fn interpret_binary_op(l: &Value, op: BinaryOp, r: &Value) -> Result<Value> {
    if crate::op_census::enabled() {
        use std::hash::{Hash, Hasher};
        let mut hasher = rustc_hash::FxHasher::default();
        std::mem::discriminant(&op).hash(&mut hasher);
        memo_price(hasher.finish(), &[l, r]);
    }
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
            // No error path: PICO-8's `%` is total. It was measured to be
            // `rem_euclid` on the raw 16.16 bits for every sign combination,
            // fractional divisors included, with `a % 0 == 0`. The lane-wise
            // failure plumbing that used to be here existed because this was
            // believed to model only a positive integer divisor.
            Ok(Value::Number(MaybeVector::map2(l, r, |l, r| *l % *r)))
        }

        // Arithmetic operations on intervals
        (Value::NumberInterval(l), BinaryOp::Plus, Value::NumberInterval(r)) => {
            Ok(Value::NumberInterval(MaybeVector::map2(l, r, |l, r| *l + *r)))
        }
        (Value::NumberInterval(l), BinaryOp::Minus, Value::NumberInterval(r)) => {
            Ok(Value::NumberInterval(MaybeVector::map2(l, r, |l, r| *l - *r)))
        }
        // Interval scaled by a POSITIVE scalar (the widened fruit-off path:
        // `off/40`, `sin(..)*2.5`). Fixed-point mul/div by a positive
        // constant is monotone, so the endpoint images bound the image of
        // every representable value in between - the result interval is
        // sound without any rounding analysis. Negative or lane-varying
        // scalars stay unsupported (loud), not silently approximated.
        (Value::NumberInterval(l), BinaryOp::Star, Value::Number(MaybeVector::Scalar(r)))
            if *r > Pico8Num::from_i16(0) =>
        {
            Ok(Value::NumberInterval(l.map(|iv| iv.scale_positive(*r))))
        }
        (Value::NumberInterval(l), BinaryOp::Slash, Value::Number(MaybeVector::Scalar(r)))
            if *r > Pico8Num::from_i16(0) =>
        {
            Ok(Value::NumberInterval(l.map(|iv| iv.div_positive(*r))))
        }

        // Interval comparisons: per-lane definite answers where the interval
        // is entirely on one side, whole-value UnknownBool as soon as any
        // lane straddles (the branch machinery then explores both arms for
        // the state - a sound over-approximation; the definite lanes are
        // re-filtered by their arms' outcomes downstream).
        (Value::NumberInterval(_), op, _) | (_, op, Value::NumberInterval(_))
            if matches!(
                op,
                BinaryOp::LessThan
                    | BinaryOp::GreaterThan
                    | BinaryOp::LessThanEqual
                    | BinaryOp::GreaterThanEqual
            ) =>
        {
            let lift = |v: &Value| -> Result<MaybeVector<celeste_core::pico8_num::Pico8NumInterval>> {
                match v {
                    Value::NumberInterval(iv) => Ok(iv.clone()),
                    Value::Number(n) => Ok(lift_to_interval(n)),
                    other => Err(anyhow!("interval comparison with {:?}", other)),
                }
            };
            let l = lift(l)?;
            let r = lift(r)?;
            // tri-state per lane: Some(bool) definite, None straddling.
            let judge = |l: &celeste_core::pico8_num::Pico8NumInterval,
                         r: &celeste_core::pico8_num::Pico8NumInterval|
             -> Option<bool> {
                match op {
                    BinaryOp::LessThan => {
                        if l.high < r.low {
                            Some(true)
                        } else if l.low >= r.high {
                            Some(false)
                        } else {
                            None
                        }
                    }
                    BinaryOp::LessThanEqual => {
                        if l.high <= r.low {
                            Some(true)
                        } else if l.low > r.high {
                            Some(false)
                        } else {
                            None
                        }
                    }
                    BinaryOp::GreaterThan => {
                        if l.low > r.high {
                            Some(true)
                        } else if l.high <= r.low {
                            Some(false)
                        } else {
                            None
                        }
                    }
                    BinaryOp::GreaterThanEqual => {
                        if l.low >= r.high {
                            Some(true)
                        } else if l.high < r.low {
                            Some(false)
                        } else {
                            None
                        }
                    }
                    _ => unreachable!(),
                }
            };
            let tri = MaybeVector::map2(&l, &r, |l, r| judge(l, r));
            let (any_unknown, all_unknown) = match &tri {
                MaybeVector::Scalar(t) => (t.is_none(), t.is_none()),
                MaybeVector::Vector(ts) => (
                    ts.iter().any(|t| t.is_none()),
                    ts.iter().all(|t| t.is_none()),
                ),
            };
            if any_unknown {
                // How much precision does collapsing to UnknownBool
                // actually cost? Partitioning the state instead only helps
                // where some lane HAS an answer, so count that directly.
                let (definite, total) = match &tri {
                    MaybeVector::Scalar(t) => (t.is_some() as usize, 1),
                    MaybeVector::Vector(ts) => {
                        (ts.iter().filter(|t| t.is_some()).count(), ts.len())
                    }
                };
                crate::op_census::record_unknown_collapse(definite, total);
            }
            if all_unknown || (any_unknown && !partition_straddles_enabled()) {
                // No lane has an answer, or some lane does not and tri-state
                // is off: collapse to the whole-value case, which is what
                // this arm did before tri-state existed and what the branch
                // machinery already handles.
                //
                // The `!partition_straddles_enabled()` term is load-bearing and was
                // missing: gating tri-state off sent MIXED comparisons into
                // the `unwrap` arm below, which panics on the first lane
                // that straddles. Room (1,0) never showed it because
                // nothing there produces a straddling comparison; room
                // (0,0)'s widened strawberry produces them constantly, and
                // it aborted the campaign at f66.
                Ok(Value::UnknownBool)
            } else if any_unknown {
                // MIXED - the case this whole change exists for. Previously
                // one straddling lane collapsed every definite answer beside
                // it, dragging the entire state onto the plain program. Now
                // the definite lanes keep their answers and only the
                // ambiguous ones are duplicated, when this transient is
                // resolved at assignment (see resolve_maybe_bool).
                Ok(Value::MaybeBool(tri))
            } else {
                // Every lane is definite - guaranteed by the arms above, and
                // the unwraps below depend on it.
                debug_assert!(!any_unknown);
                let bools = match &tri {
                    MaybeVector::Scalar(t) => MaybeVector::Scalar(
                        t.expect("an all-definite comparison has no straddling lane"),
                    ),
                    MaybeVector::Vector(ts) => MaybeVector::vector(
                        ts.iter()
                            .map(|t| {
                                t.expect("an all-definite comparison has no straddling lane")
                            })
                            .collect(),
                    ),
                };
                Ok(Value::Bool(bools))
            }
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
    use celeste_core::pico8_num::Pico8Num;

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
    fn test_not_number_returns_error() {
        let result = interpret_not(&num(5));
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("Unsupported value for not"),
            "Expected error message to contain 'Unsupported value for not', got: {}",
            err_msg
        );
    }

    #[test]
    fn test_not_vector() {
        let v = Value::Bool(MaybeVector::vector(vec![true, false, true]));
        let result = interpret_not(&v).unwrap();
        assert_eq!(
            result,
            Value::Bool(MaybeVector::vector(vec![false, true, false]))
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
        let a = Value::Number(MaybeVector::vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(3),
        ]));
        let b = Value::Number(MaybeVector::vector(vec![
            Pico8Num::from_i16(10),
            Pico8Num::from_i16(20),
            Pico8Num::from_i16(30),
        ]));
        let result = interpret_binary_op(&a, BinaryOp::Plus, &b).unwrap();
        assert_eq!(
            result,
            Value::Number(MaybeVector::vector(vec![
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
        let vector = Value::Number(MaybeVector::vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(3),
        ]));
        let result = interpret_binary_op(&scalar, BinaryOp::Plus, &vector).unwrap();
        assert_eq!(
            result,
            Value::Number(MaybeVector::vector(vec![
                Pico8Num::from_i16(11),
                Pico8Num::from_i16(12),
                Pico8Num::from_i16(13),
            ]))
        );
    }

    #[test]
    fn test_compare_vectors() {
        let a = Value::Number(MaybeVector::vector(vec![
            Pico8Num::from_i16(1),
            Pico8Num::from_i16(5),
            Pico8Num::from_i16(3),
        ]));
        let b = Value::Number(MaybeVector::vector(vec![
            Pico8Num::from_i16(2),
            Pico8Num::from_i16(4),
            Pico8Num::from_i16(3),
        ]));
        let result = interpret_binary_op(&a, BinaryOp::LessThan, &b).unwrap();
        // 1<2=true, 5<4=false, 3<3=false
        assert_eq!(
            result,
            Value::Bool(MaybeVector::vector(vec![true, false, false]))
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

#[cfg(test)]
mod select_tests {
    use super::*;

    fn num(n: i16) -> Value {
        Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(n)))
    }

    fn nums(ns: &[i16]) -> Value {
        Value::Number(MaybeVector::vector(
            ns.iter().map(|n| Pico8Num::from_i16(*n)).collect(),
        ))
    }

    fn bools(bs: &[bool]) -> Value {
        Value::Bool(MaybeVector::vector(bs.to_vec()))
    }

    #[test]
    fn picks_per_lane_and_broadcasts_scalars() {
        let got = interpret_select(&bools(&[true, false, true]), &nums(&[1, 2, 3]), &num(9))
            .unwrap();
        assert_eq!(got, nums(&[1, 9, 3]));
    }

    /// A uniform condition needs no per-lane combining, which is what lets a
    /// select survive on values it could not otherwise represent.
    #[test]
    fn a_uniform_condition_passes_a_value_through_untouched() {
        let pointer = Value::Pointer(crate::interpreter::heap::HeapId::from_raw(7));
        let got = interpret_select(
            &Value::Bool(MaybeVector::Scalar(true)),
            &pointer,
            &Value::String("other".to_string()),
        )
        .unwrap();
        assert_eq!(got, pointer);

        // Same when the condition is a vector that happens to be uniform.
        let got = interpret_select(&bools(&[false, false]), &num(1), &pointer).unwrap();
        assert_eq!(got, pointer);
    }

    /// The case `if_convert` runs into in the real program: Lua's `and`/`or`
    /// returns its operands, so a join can carry a bool on one side and a
    /// number on the other. There is no per-lane representation for that, and
    /// widening it would silently lose information.
    #[test]
    fn refuses_to_combine_a_bool_and_a_number() {
        let err = interpret_select(&bools(&[true, false]), &num(1), &bools(&[false, false]))
            .unwrap_err();
        assert!(
            format!("{}", err).contains("cannot combine"),
            "{}",
            err
        );
    }

    /// `UnknownBool` sends a *branch* down both edges unfiltered, so there is
    /// no lane-wise answer for a select to give.
    #[test]
    fn refuses_an_unknown_condition() {
        let err = interpret_select(&Value::UnknownBool, &num(1), &num(2)).unwrap_err();
        assert!(format!("{}", err).contains("no per-lane value"), "{}", err);
    }

    #[test]
    fn equal_values_need_no_combining() {
        let s = Value::String("x".to_string());
        assert_eq!(
            interpret_select(&bools(&[true, false]), &s, &s).unwrap(),
            s
        );
    }

    /// `%` never fails and never panics, for ANY operands.
    ///
    /// This test used to assert that a zero divisor is an error, because `%`
    /// was believed to be modelled only for a positive integer divisor. A
    /// real console says otherwise: `a % b` is `rem_euclid` on the raw 16.16
    /// bits for every combination, and `a % 0` is 0. So the case that used
    /// to be "out of range" is simply an answer.
    ///
    /// The property that mattered is stronger now rather than gone.
    /// `if_convert` deliberately runs arithmetic on lanes that would not have
    /// reached it, so `%` must not blow up on operands the real program would
    /// never produce - and now there is no input at all that it rejects.
    #[test]
    fn modulo_is_total_and_matches_the_console() {
        let cases = [(-2, 0, 0), (7, 0, 0), (-2, 8, 6), (7, -3, 1), (-7, 3, 2), (10, 8, 2)];
        for (a, b, want) in cases {
            let got = interpret_binary_op(&num(a), BinaryOp::Percent, &num(b))
                .unwrap_or_else(|e| panic!("{} % {} failed: {}", a, b, e));
            assert_eq!(
                format!("{:?}", got),
                format!("{:?}", num(want)),
                "{} % {}",
                a,
                b
            );
        }
    }
}

#[cfg(test)]
mod dict_pricing {
    use celeste_core::pico8_num::Pico8Num;
    use rustc_hash::FxHashMap;

    /// Prices per-context evaluation against direct per-lane evaluation, at
    /// the state set's real cardinalities (state-structure.md: <=59 distinct
    /// values per column, ~1500 joint contexts). Three strategies:
    ///
    ///   direct:   out[i] = f(a[i], b[i])                (what the interpreter does)
    ///   dict:     build pair codes on the fly, eval each distinct pair once,
    ///             gather                                (per-op dictionary)
    ///   coded:    inputs already carry u8 codes + value tables; combine
    ///             codes, eval the tiny table, gather    (persistent representation)
    ///
    /// Run with: cargo test --release dict_pricing -- --ignored --nocapture
    #[test]
    #[ignore]
    fn bench_dict_vs_direct() {
        const N: usize = 1 << 21;
        let mut seed = 0x9e3779b97f4a7c15u64;
        let mut next = move || {
            seed ^= seed << 13;
            seed ^= seed >> 7;
            seed ^= seed << 17;
            seed
        };

        for &(ka, kb) in &[(5usize, 7usize), (13, 20), (51, 59)] {
            let pool_a: Vec<Pico8Num> = (0..ka).map(|i| Pico8Num::from_i16(i as i16 + 1)).collect();
            let pool_b: Vec<Pico8Num> = (0..kb).map(|i| Pico8Num::from_i16(i as i16 + 1)).collect();
            let codes_a: Vec<u8> = (0..N).map(|_| (next() as usize % ka) as u8).collect();
            let codes_b: Vec<u8> = (0..N).map(|_| (next() as usize % kb) as u8).collect();
            let a: Vec<Pico8Num> = codes_a.iter().map(|&c| pool_a[c as usize]).collect();
            let b: Vec<Pico8Num> = codes_b.iter().map(|&c| pool_b[c as usize]).collect();

            // Macro rather than a fn pointer: an indirect call per lane
            // blocks inlining and vectorization, and pessimized the direct
            // baseline 4.5x in the first version of this benchmark (the
            // interpreter's map2 inlines its closure, so the honest
            // baseline must too).
            macro_rules! bench_op {
                ($op_name:literal, $f:expr) => {{
                let op_name = $op_name;
                let f = $f;
                let t = std::time::Instant::now();
                let direct: Vec<Pico8Num> = a.iter().zip(&b).map(|(&x, &y)| f(x, y)).collect();
                let t_direct = t.elapsed();

                let t = std::time::Instant::now();
                let mut dict: FxHashMap<(Pico8Num, Pico8Num), u16> = FxHashMap::default();
                let mut values: Vec<Pico8Num> = Vec::new();
                let mut codes: Vec<u16> = Vec::with_capacity(N);
                for (&x, &y) in a.iter().zip(&b) {
                    let next_code = values.len() as u16;
                    let code = *dict.entry((x, y)).or_insert_with(|| {
                        values.push(f(x, y));
                        next_code
                    });
                    codes.push(code);
                }
                let dict_out: Vec<Pico8Num> = codes.iter().map(|&c| values[c as usize]).collect();
                let t_dict = t.elapsed();

                let t = std::time::Instant::now();
                let mut table: Vec<Pico8Num> = Vec::with_capacity(ka * kb);
                for &x in &pool_a {
                    for &y in &pool_b {
                        table.push(f(x, y));
                    }
                }
                let coded_out: Vec<Pico8Num> = codes_a
                    .iter()
                    .zip(&codes_b)
                    .map(|(&ca, &cb)| table[ca as usize * kb + cb as usize])
                    .collect();
                let t_coded = t.elapsed();

                assert_eq!(direct, dict_out);
                assert_eq!(direct, coded_out);
                println!(
                    "{op_name:>4} ka={ka:>2} kb={kb:>2}: direct {:>7.2?}  dict {:>7.2?} ({:.2}x)  coded {:>7.2?} ({:.2}x)",
                    t_direct,
                    t_dict,
                    t_dict.as_secs_f64() / t_direct.as_secs_f64(),
                    t_coded,
                    t_coded.as_secs_f64() / t_direct.as_secs_f64(),
                );
                }};
            }
            bench_op!("add", |x: Pico8Num, y: Pico8Num| x + y);
            bench_op!("div", |x: Pico8Num, y: Pico8Num| x / y);
        }
    }
}
