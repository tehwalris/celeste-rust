use std::sync::Arc;

use serde::{Deserialize, Serialize};

use crate::ir::{LocalId, SlotMap};

use super::value::Value;

const NO_OCCUPANT: u32 = u32::MAX;

#[derive(Debug, Clone, PartialEq, Eq)]
struct EnvData {
    values: Vec<Option<Value>>,
    /// Which `LocalId` currently lives in each slot.
    ///
    /// This is the runtime half of the correctness story for slot allocation.
    /// A bad allocation puts two simultaneously-live values in one slot; the
    /// reader of the clobbered one then finds the wrong occupant and we fail
    /// loudly instead of silently computing nonsense. It also catches
    /// interpreter-level hazards no static check can see - notably that
    /// `flow_block_phi` assigns phis sequentially, which is wrong if allocation
    /// makes two phis in a block swap slots.
    occupant: Vec<u32>,
}

/// A local environment storing local variable bindings.
///
/// Copy-on-write via `Arc` for cheap cloning. Indexed by *slot*, not by
/// `LocalId` - see `SlotMap`.
#[derive(Clone, Debug)]
pub struct LocalEnv {
    slots: Arc<SlotMap>,
    data: Arc<EnvData>,
}

impl PartialEq for LocalEnv {
    fn eq(&self, other: &Self) -> bool {
        if Arc::ptr_eq(&self.data, &other.data) {
            return true;
        }
        self.data == other.data
    }
}

impl Eq for LocalEnv {}

impl Serialize for LocalEnv {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        // Serialised by occupant, not by slot, so a dump stays meaningful
        // across reallocation.
        let pairs: Vec<(LocalId, Value)> = self
            .data
            .values
            .iter()
            .enumerate()
            .filter_map(|(slot, v)| {
                let value = v.as_ref()?;
                let occupant = *self.data.occupant.get(slot)?;
                if occupant == NO_OCCUPANT {
                    return None;
                }
                Some((LocalId::from(occupant as usize), value.clone()))
            })
            .collect();
        pairs.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for LocalEnv {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let pairs: Vec<(LocalId, Value)> = Vec::deserialize(deserializer)?;
        // Restored under the identity map; a state dump does not record which
        // allocation produced it.
        let mut env = LocalEnv::new();
        for (id, value) in pairs {
            env.set(id, value);
        }
        Ok(env)
    }
}

impl Default for LocalEnv {
    fn default() -> Self {
        Self::new()
    }
}

impl LocalEnv {
    /// An environment under the identity slot map.
    pub fn new() -> Self {
        Self::with_slots(Arc::new(SlotMap::identity()))
    }

    pub fn with_slots(slots: Arc<SlotMap>) -> Self {
        let n = slots.num_slots();
        Self {
            slots,
            data: Arc::new(EnvData {
                values: vec![None; n],
                occupant: vec![NO_OCCUPANT; n],
            }),
        }
    }

    /// An empty environment with the same slot map as this one.
    pub fn empty_like(&self) -> Self {
        Self::with_slots(Arc::clone(&self.slots))
    }

    pub fn slots(&self) -> &Arc<SlotMap> {
        &self.slots
    }

    /// True when no slot holds a value. Note that an environment stays empty
    /// across a `Return` with no value, which is what lets the frame chunk hand
    /// its state to the next frame.
    pub fn is_empty(&self) -> bool {
        self.data.values.iter().all(|v| v.is_none())
    }

    pub fn with_capacity(_max_locals: usize) -> Self {
        Self::new()
    }

    #[inline]
    pub fn get(&self, id: LocalId) -> &Value {
        let slot = self.slots.slot_of(id);
        match self.data.occupant.get(slot) {
            Some(&occupant) if occupant == usize::from(id) as u32 => self
                .data
                .values
                .get(slot)
                .and_then(|v| v.as_ref())
                .expect("occupied slot must hold a value"),
            Some(&occupant) if occupant == NO_OCCUPANT => {
                panic!("LocalId %{} should be set before get", usize::from(id))
            }
            Some(&occupant) => panic!(
                "slot {} holds %{} but %{} was requested - the slot allocation \
                 puts two simultaneously live values in the same slot",
                slot,
                occupant,
                usize::from(id)
            ),
            None => panic!("LocalId %{} should be set before get", usize::from(id)),
        }
    }

    #[inline]
    pub fn set(&mut self, id: LocalId, value: Value) {
        let slot = self.slots.slot_of(id);
        let data = Arc::make_mut(&mut self.data);
        if slot >= data.values.len() {
            data.values.resize(slot + 1, None);
            data.occupant.resize(slot + 1, NO_OCCUPANT);
        }
        data.values[slot] = Some(value);
        data.occupant[slot] = usize::from(id) as u32;
    }

    pub fn retain(&mut self, f: impl Fn(LocalId) -> bool) {
        let data = Arc::make_mut(&mut self.data);
        for (slot, v) in data.values.iter_mut().enumerate() {
            if v.is_none() {
                continue;
            }
            let occupant = data.occupant[slot];
            if occupant == NO_OCCUPANT || !f(LocalId::from(occupant as usize)) {
                *v = None;
                data.occupant[slot] = NO_OCCUPANT;
            }
        }
    }

    /// Drops one local, if it is still the occupant of its slot.
    ///
    /// A slot whose occupant has moved on already lost this value - slots are
    /// shared between locals with disjoint live ranges - so that case is a
    /// no-op. What must not happen is dropping a *different* local's value,
    /// which the occupant check prevents.
    pub fn kill(&mut self, id: LocalId) {
        let slot = self.slots.slot_of(id);
        let Some(&occupant) = self.data.occupant.get(slot) else {
            return;
        };
        if occupant != usize::from(id) as u32 {
            return;
        }
        let data = Arc::make_mut(&mut self.data);
        data.values[slot] = None;
        data.occupant[slot] = NO_OCCUPANT;
    }

    pub fn clear(&mut self) {
        self.data = Arc::new(EnvData {
            values: vec![None; self.slots.num_slots()],
            occupant: vec![NO_OCCUPANT; self.slots.num_slots()],
        });
    }

    #[inline]
    pub fn map_in_place(&mut self, f: impl Fn(Value) -> Value) {
        let data = Arc::make_mut(&mut self.data);
        for v in data.values.iter_mut() {
            if let Some(val) = v.take() {
                *v = Some(f(val));
            }
        }
    }

    /// Filter vectors down to the `kept` lanes, only transforming values
    /// that are vectors.
    #[inline]
    pub fn filter_vectors_in_place(&mut self, kept: &super::value::KeptLanes) {
        let data = Arc::make_mut(&mut self.data);
        for v in data.values.iter_mut() {
            if let Some(val) = v.as_ref() {
                if let Some(new_val) = val.filter_vectors_if_vector(kept) {
                    *v = Some(new_val);
                }
            }
        }
    }

    /// Split vector values into two environments in one walk: `self` keeps
    /// the matching lanes, `other` (a clone of the pre-split env) gets the
    /// rest. Scalars stay shared. Occupancy is untouched on both sides.
    pub fn split_vectors_in_place(&mut self, other: &mut Self, runs: &super::value::SplitRuns) {
        let data_a = Arc::make_mut(&mut self.data);
        let data_b = Arc::make_mut(&mut other.data);
        debug_assert_eq!(data_a.values.len(), data_b.values.len(), "split of diverged envs");
        for (slot_a, slot_b) in data_a.values.iter_mut().zip(data_b.values.iter_mut()) {
            if let Some(value) = slot_a.as_ref() {
                if let Some((a, b)) = value.split_vectors_if_vector(runs) {
                    *slot_a = Some(a);
                    *slot_b = Some(b);
                }
            }
        }
    }

    /// Iterate over occupied `(slot, value)` pairs.
    ///
    /// Yields slots, not `LocalId`s. Callers that rebuild an environment
    /// (garbage collection, vectorisation) work positionally and must preserve
    /// occupancy - use `set_slot` for that.
    pub fn iter(&self) -> impl Iterator<Item = (usize, &Value)> {
        self.data
            .values
            .iter()
            .enumerate()
            .filter_map(|(i, v)| v.as_ref().map(|v| (i, v)))
    }

    /// The `LocalId` currently occupying a slot, if any.
    pub fn occupant_of_slot(&self, slot: usize) -> Option<LocalId> {
        match self.data.occupant.get(slot) {
            Some(&o) if o != NO_OCCUPANT => Some(LocalId::from(o as usize)),
            _ => None,
        }
    }

    /// Positional read, bypassing the occupant check. For code that rebuilds an
    /// environment slot by slot.
    #[inline]
    pub fn get_by_raw_id(&self, slot: usize) -> &Value {
        self.data
            .values
            .get(slot)
            .and_then(|v| v.as_ref())
            .expect("slot should be set before get")
    }

    /// Positional write that preserves the slot's existing occupant. Used when
    /// rebuilding an environment from another one.
    #[inline]
    pub fn set_by_raw_id(&mut self, slot: usize, value: Value) {
        let data = Arc::make_mut(&mut self.data);
        if slot >= data.values.len() {
            data.values.resize(slot + 1, None);
            data.occupant.resize(slot + 1, NO_OCCUPANT);
        }
        data.values[slot] = Some(value);
    }

    /// Positional write that also records who owns the slot.
    #[inline]
    pub fn set_slot(&mut self, slot: usize, occupant: Option<LocalId>, value: Value) {
        self.set_by_raw_id(slot, value);
        let data = Arc::make_mut(&mut self.data);
        data.occupant[slot] = occupant.map_or(NO_OCCUPANT, |id| usize::from(id) as u32);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::pico8_num::Pico8Num;
    use crate::interpreter::value::MaybeVector;

    fn num(n: i16) -> Value {
        Value::Number(MaybeVector::Scalar(Pico8Num::from_i16(n)))
    }

    #[test]
    fn identity_map_behaves_like_the_old_env() {
        let mut env = LocalEnv::new();
        env.set(LocalId::from(3), num(7));
        assert_eq!(env.get(LocalId::from(3)), &num(7));
        assert_eq!(env.iter().count(), 1);
    }

    #[test]
    fn a_shared_slot_is_fine_when_the_values_do_not_overlap() {
        // %0 and %1 both live in slot 0, but %0 is overwritten before %1 is read.
        let slots = Arc::new(SlotMap::from_vec(vec![0, 0]));
        let mut env = LocalEnv::with_slots(slots);
        env.set(LocalId::from(0), num(1));
        assert_eq!(env.get(LocalId::from(0)), &num(1));
        env.set(LocalId::from(1), num(2));
        assert_eq!(env.get(LocalId::from(1)), &num(2));
    }

    /// The runtime half of slot-allocation correctness: reading a value that a
    /// later definition has clobbered must fail loudly, not return the wrong
    /// number.
    #[test]
    #[should_panic(expected = "two simultaneously live values")]
    fn reading_a_clobbered_value_panics() {
        let slots = Arc::new(SlotMap::from_vec(vec![0, 0]));
        let mut env = LocalEnv::with_slots(slots);
        env.set(LocalId::from(0), num(1));
        env.set(LocalId::from(1), num(2));
        let _ = env.get(LocalId::from(0));
    }
}
