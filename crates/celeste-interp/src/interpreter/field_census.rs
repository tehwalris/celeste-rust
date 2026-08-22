//! Which state FIELD carries the lane multiplicity? (`rewrite field-census`)
//!
//! Room (2,0)'s geometry saturates - ~4,500 reachable whole-pixel positions,
//! 96% of them occupied by f050 - so every later doubling of the frontier is
//! state AT a position (see `plans/room20-plan.md`). That says the ladder
//! needs a rung that abstracts something other than `player.rem`, but not
//! WHICH field, and a rung built on the wrong field buys nothing.
//!
//! This census answers it with numbers, offline, from the boundary states a
//! forward pass already saved (`bench --save-frames`), so it costs no search
//! time and cannot contaminate a benchmark. For one frame's boundary states
//! it reports, per lane-varying field:
//!
//!   * how many distinct values the field takes over the whole frontier;
//!   * `rows_after / rows_before` if that field were collapsed to a single
//!     value - the crudest possible bucketing, and therefore an UPPER BOUND
//!     on what any rung abstracting that field could merge;
//!   * the same for arbitrary SETS of fields (`--collapse`), since a rung
//!     that buckets `spd.x` and `spd.y` collapses both at once, and for
//!     BUCKETING rather than erasing them (`--collapse player.spd.x:2`,
//!     the fraction-bit count `CELESTE_REM_BITS` uses) - which is what a
//!     real rung would do, and is strictly weaker than erasing;
//!   * per whole-pixel position, how many distinct values each field takes
//!     (`--out`, one CSV row per occupied position), so the product of the
//!     per-field counts can be compared against the actual lane count there
//!     and correlated fields are visible as a large gap.
//!
//! Method. A "row" is one lane's complete canonical value tuple, exactly as
//! `visited_row_keys` keys it. States are grouped by shape (rows of
//! different shapes are never equal), each group is canonicalized with
//! `collect_columns_labeled`, and each lane-varying column is folded into
//! the row key by XOR of a per-field hash instead of the interpreter's
//! order-dependent fold. XOR is what makes "collapse field F" cost one pass:
//! `row ^ contribution_F` is the row key with F removed. Counting is
//! hash-only (64-bit), so a collision undercounts distinct rows by ~1e-6 at
//! this scale - fine for a census, and never used as a search input.
//!
//! Columns are named by their field path (`player.spd.x`) via
//! `merge_dump::cell_names`, with the player's own subtree renamed from
//! `objects.<i>.…` to `player.…` so the same field has the same name in
//! every object-array shape. A field that is constant across a shape group
//! is left out: it cannot tell two rows of that shape apart, so it changes
//! no count here. `__button_states` is such a field at every boundary -
//! `__reset_button_states()` makes it fresh-unknown at the end of the frame
//! chunk - which is why the input a lane took shows up only through the
//! player's own `p_jump`/`p_dash` copies of it.
//!
//! Two invariants make the output checkable, and both are worth re-running
//! after any change here:
//!
//!   * `rows_before` must equal the frontier's lane count exactly, because a
//!     frontier-only boundary contains no duplicate rows;
//!   * collapsing EVERY lane-varying field must leave exactly one row per
//!     object-array shape.
//!
//! Bucketing is a mask on the 16.16 fixed-point bits, so `field:n` on a
//! BOOLEAN column erases it for every `n < 16` - which is what one wants
//! (there is nothing between "both" and "exact" for a bool), but is worth
//! knowing when reading a mixed set's name.

use std::collections::{BTreeMap, HashMap};
use std::io::Write;

use anyhow::{anyhow, Result};
use rustc_hash::{FxHashMap, FxHashSet};

use super::heap::HeapId;
use super::inspect::StateHelper;
use super::merge_dump::cell_names;
use super::state::State;
use super::value::{HeapValue, Value};
use super::vectorize::shape_hash_of_state;
use super::virtual_merge::{collect_columns_labeled, Column, Origin, Piece};

/// Splitmix64 finalizer.
fn mix(mut x: u64) -> u64 {
    x = (x ^ (x >> 30)).wrapping_mul(0xbf58_476d_1ce4_e5b9);
    x = (x ^ (x >> 27)).wrapping_mul(0x94d0_49bb_1331_11eb);
    x ^ (x >> 31)
}

/// A field's XOR contribution to the row key. The seed is derived from the
/// field NAME, not from its column index, so a field that exists in one
/// shape and not another still contributes the same way wherever it exists.
fn contribution(seed: u64, key: u64) -> u64 {
    mix(key ^ seed)
}

fn name_seed(label: &str) -> u64 {
    use std::hash::{Hash, Hasher};
    let mut h = rustc_hash::FxHasher::default();
    label.hash(&mut h);
    mix(h.finish() | 1)
}

/// One column's per-lane value, widened to a u64 key. Equality of keys is
/// exactly value equality (`Pico8Num::to_bits`); intervals pack both ends.
fn column_keys(column: &Column, rows: usize) -> Vec<u64> {
    let mut out: Vec<u64> = Vec::with_capacity(rows);
    match column {
        Column::Numbers(pieces) => {
            for piece in pieces {
                match piece {
                    Piece::Slice(s) => out.extend(s.iter().map(|v| v.to_bits() as u64)),
                    Piece::Scalar(v, n) => {
                        out.extend(std::iter::repeat(v.to_bits() as u64).take(*n))
                    }
                }
            }
        }
        Column::Bools(pieces) => {
            for piece in pieces {
                match piece {
                    Piece::Slice(s) => out.extend(s.iter().map(|v| *v as u64)),
                    Piece::Scalar(v, n) => out.extend(std::iter::repeat(*v as u64).take(*n)),
                }
            }
        }
        Column::Intervals(pieces) => {
            let pack = |v: &celeste_core::pico8_num::Pico8NumInterval| {
                (v.low.to_bits() as u64) | ((v.high.to_bits() as u64) << 32)
            };
            for piece in pieces {
                match piece {
                    Piece::Slice(s) => out.extend(s.iter().map(pack)),
                    Piece::Scalar(v, n) => out.extend(std::iter::repeat(pack(v)).take(*n)),
                }
            }
        }
    }
    debug_assert_eq!(out.len(), rows);
    out
}

/// Rename the player object's subtree to `player.…`, so `player.spd.x` is
/// one field across shapes whose object arrays put the player at different
/// indices. Leaves every other cell with its `cell_names` path.
fn rename_player_subtree(state: &State, names: &mut HashMap<usize, String>) {
    let helper = StateHelper::new(state);
    let Some(arr) = helper.get_objects_array_id() else { return };
    let Some(player) = helper
        .find_objects_by_type(arr, "player")
        .ok()
        .and_then(|v| v.first().copied())
    else {
        return;
    };
    let mut seen: FxHashSet<usize> = FxHashSet::default();
    let mut queue: Vec<(HeapId, String, usize)> = vec![(player, "player".to_string(), 0)];
    while let Some((id, path, depth)) = queue.pop() {
        if depth > 3 || !seen.insert(id.raw()) {
            continue;
        }
        match helper.load(id) {
            HeapValue::ObjectTable(fields) => {
                for (field, child) in fields.iter() {
                    let label = format!("{}.{}", path, field);
                    names.insert(child.raw(), label.clone());
                    queue.push((*child, label, depth + 1));
                }
            }
            // A pointer cell keeps its own name; its target's FIELDS get it
            // as their prefix, which is how `player.spd` reaches
            // `player.spd.x`.
            HeapValue::Value(Value::Pointer(target)) => queue.push((*target, path, depth)),
            _ => {}
        }
    }
}

/// One shape's canonicalized lanes: the row key per lane, the packed
/// whole-pixel position per lane, and every lane-varying field's values.
struct Group {
    rows: usize,
    row_hash: Vec<u64>,
    /// `(x, y)` per lane, `None` when this shape has no player object.
    pos: Option<Vec<(i16, i16)>>,
    /// Field label -> per-lane value keys, for lane-varying fields only.
    fields: FxHashMap<String, Vec<u64>>,
}

fn distinct(values: impl Iterator<Item = u64>) -> usize {
    let set: FxHashSet<u64> = values.collect();
    set.len()
}

fn build_group(states: &[State]) -> Result<Group> {
    let rows: usize = states.iter().map(|s| s.vector_size).sum();
    let (columns, origins) = collect_columns_labeled(states)
        .ok_or_else(|| anyhow!("collect_columns_labeled failed on a shape group"))?;
    let mut names = cell_names(&states[0]);
    rename_player_subtree(&states[0], &mut names);
    let label_of = |origin: &Origin| -> String {
        match origin {
            Origin::Heap(cell) => names
                .get(cell)
                .cloned()
                .unwrap_or_else(|| format!("cell{}", cell)),
            Origin::HeapCapture(cell, k) => format!("cell{}.capture{}", cell, k),
            Origin::Local(slot) => format!("local{}", slot),
            Origin::Outer(d, slot) => format!("outer{}.local{}", d, slot),
        }
    };

    // Position first: needed even when `player.x` happens to be uniform.
    let mut xy: [Option<Vec<u64>>; 2] = [None, None];
    for (column, origin) in columns.iter().zip(&origins) {
        let label = label_of(origin);
        let slot = match label.as_str() {
            "player.x" => 0,
            "player.y" => 1,
            _ => continue,
        };
        xy[slot] = Some(column_keys(column, rows));
    }
    let pos = match (&xy[0], &xy[1]) {
        (Some(xs), Some(ys)) => Some(
            xs.iter()
                .zip(ys)
                // Low 32 bits are the value's fixed-point bits (an interval
                // column packs its LOW end there); >> 16 is the whole part.
                .map(|(x, y)| {
                    let whole = |k: &u64| ((*k as u32) as i32 >> 16) as i16;
                    (whole(x), whole(y))
                })
                .collect(),
        ),
        _ => None,
    };

    // Lane-varying fields, folded into the row key. `cell_names` can hand
    // two cells the same path; those get `#2`, `#3` rather than being merged,
    // so every field keeps its RAW values and stays individually bucketable.
    let mut fields: FxHashMap<String, Vec<u64>> = FxHashMap::default();
    for (column, origin) in columns.iter().zip(&origins) {
        if column.is_uniform() {
            // Constant across the whole group: it cannot tell two rows
            // apart, so it changes no count in this census.
            continue;
        }
        let base = label_of(origin);
        let mut label = base.clone();
        let mut n = 1;
        while fields.contains_key(&label) {
            n += 1;
            label = format!("{}#{}", base, n);
        }
        fields.insert(label, column_keys(column, rows));
    }

    let mut row_hash = vec![0u64; rows];
    for (label, keys) in &fields {
        let seed = name_seed(label);
        for (row, key) in row_hash.iter_mut().zip(keys) {
            *row ^= contribution(seed, *key);
        }
    }
    Ok(Group { rows, row_hash, pos, fields })
}

/// What a rung would do to one field: erase it entirely (the upper bound on
/// any abstraction of it), or floor it to a bucket of width `2^-bits` in the
/// 16.16 fixed-point representation - bit for bit the bucketing the `rem`
/// ladder applies (`abstraction::rem_bucket` is `div_euclid(width) * width`,
/// which on two's complement is exactly this mask).
#[derive(Clone, Copy)]
pub enum Collapse {
    Drop,
    /// Fraction bits kept, as in `CELESTE_REM_BITS`. 0 keeps only the whole
    /// part; 16 would be exact and is rejected by the parser.
    Bits(u8),
}

impl Collapse {
    fn apply(&self, key: u64) -> Option<u64> {
        match self {
            Collapse::Drop => None,
            Collapse::Bits(bits) => {
                let width = 0x1_0000u64 >> bits;
                Some(key & !(width - 1))
            }
        }
    }
}

/// Sum over shapes of the distinct row keys, with `ops` applied. Rows of
/// different shapes are never equal, so summing over shapes is exact.
fn rows_after(groups: &[Group], ops: &[(String, Collapse)]) -> usize {
    let mut total = 0usize;
    for group in groups {
        let mut hashes = group.row_hash.clone();
        for (label, how) in ops {
            let Some(keys) = group.fields.get(label) else { continue };
            let seed = name_seed(label);
            for (row, key) in hashes.iter_mut().zip(keys) {
                *row ^= contribution(seed, *key);
                if let Some(bucketed) = how.apply(*key) {
                    *row ^= contribution(seed, bucketed);
                }
            }
        }
        total += distinct(hashes.into_iter());
    }
    total
}

/// One "what would a rung buy" question: a set of fields collapsed at once.
pub struct CollapseSet {
    pub ops: Vec<(String, Collapse)>,
}

impl CollapseSet {
    /// `field[:bits],field[:bits],…`, where the bare form erases the field.
    pub fn parse(spec: &str) -> Result<Self> {
        let mut ops = Vec::new();
        for entry in spec.split(',') {
            let entry = entry.trim();
            if entry.is_empty() {
                continue;
            }
            match entry.rsplit_once(':') {
                None => ops.push((entry.to_string(), Collapse::Drop)),
                Some((name, bits)) => {
                    let bits: u8 = bits.parse().map_err(|_| {
                        anyhow!("{:?}: {:?} is not a fraction-bit count", entry, bits)
                    })?;
                    if bits >= 16 {
                        return Err(anyhow!(
                            "{}: {} fraction bits is exact, which merges nothing",
                            name,
                            bits
                        ));
                    }
                    ops.push((name.to_string(), Collapse::Bits(bits)));
                }
            }
        }
        if ops.is_empty() {
            return Err(anyhow!("empty collapse set {:?}", spec));
        }
        Ok(Self { ops })
    }

    fn name(&self) -> String {
        self.ops
            .iter()
            .map(|(l, how)| match how {
                Collapse::Drop => l.clone(),
                Collapse::Bits(b) => format!("{}:{}", l, b),
            })
            .collect::<Vec<_>>()
            .join("+")
    }
}

/// `rows_after` for a set of fields erased outright.
fn rows_with_dropped(groups: &[Group], drop: &[String]) -> usize {
    let ops: Vec<(String, Collapse)> =
        drop.iter().map(|l| (l.clone(), Collapse::Drop)).collect();
    rows_after(groups, &ops)
}

/// Distinct values a set of fields jointly takes at each occupied position,
/// counted AFTER `ops` bucketing (an erased field contributes nothing, so a
/// set of erased fields reports 1 everywhere - only bucketed sets are
/// interesting here). Returns `position -> distinct count`.
fn per_position_distinct(
    groups: &[Group],
    ops: &[(String, Collapse)],
) -> BTreeMap<(i16, i16), usize> {
    let mut pairs: Vec<((i16, i16), u64)> = Vec::new();
    for group in groups {
        let Some(pos) = group.pos.as_ref() else { continue };
        let mut folded: Option<Vec<u64>> = None;
        for (label, how) in ops {
            let Some(keys) = group.fields.get(label) else { continue };
            let seed = name_seed(label);
            // `Drop` is measured as "how many values does it take", so the
            // per-position count uses the RAW value; `Bits` uses the bucket.
            let value = |k: &u64| match how {
                Collapse::Drop => contribution(seed, *k),
                Collapse::Bits(_) => contribution(seed, how.apply(*k).unwrap()),
            };
            match folded.as_mut() {
                None => folded = Some(keys.iter().map(value).collect()),
                Some(f) => {
                    for (slot, key) in f.iter_mut().zip(keys) {
                        *slot ^= value(key);
                    }
                }
            }
        }
        // A field absent from this shape is constant in it, so every lane
        // of this shape contributes one value at its position.
        let folded = folded.unwrap_or_else(|| vec![0u64; group.rows]);
        pairs.extend(pos.iter().copied().zip(folded));
    }
    pairs.sort_unstable();
    pairs.dedup();
    let mut out: BTreeMap<(i16, i16), usize> = BTreeMap::new();
    for (p, _) in pairs {
        *out.entry(p).or_default() += 1;
    }
    out
}

/// Lanes and distinct rows per occupied position.
fn per_position_rows(groups: &[Group]) -> BTreeMap<(i16, i16), (usize, usize)> {
    let mut lanes: BTreeMap<(i16, i16), usize> = BTreeMap::new();
    let mut pairs: Vec<((i16, i16), u64)> = Vec::new();
    for group in groups {
        let Some(pos) = group.pos.as_ref() else { continue };
        for (p, h) in pos.iter().zip(&group.row_hash) {
            *lanes.entry(*p).or_default() += 1;
            pairs.push((*p, *h));
        }
    }
    pairs.sort_unstable();
    pairs.dedup();
    let mut rows: BTreeMap<(i16, i16), usize> = BTreeMap::new();
    for (p, _) in pairs {
        *rows.entry(p).or_default() += 1;
    }
    lanes
        .into_iter()
        .map(|(p, l)| {
            let r = rows.get(&p).copied().unwrap_or(0);
            (p, (l, r))
        })
        .collect()
}

/// Run the census over one frame's boundary states.
///
/// Each set in `sets` is collapsed as a unit - that is what a rung does -
/// and also gets a joint per-position column in the CSV. Every individual
/// field is always measured on its own as well.
pub fn run(
    states: Vec<State>,
    frame: u32,
    sets: &[CollapseSet],
    out_csv: Option<&std::path::Path>,
) -> Result<()> {
    let lanes: usize = states.iter().map(|s| s.vector_size).sum();
    let state_count = states.len();
    let mut by_shape: FxHashMap<u64, Vec<State>> = FxHashMap::default();
    for state in states {
        by_shape.entry(shape_hash_of_state(&state)).or_default().push(state);
    }
    let mut groups: Vec<Group> = Vec::new();
    for (_, states) in by_shape {
        groups.push(build_group(&states)?);
    }
    groups.sort_by_key(|g| std::cmp::Reverse(g.rows));

    let rows_before: usize = groups.iter().map(|g| distinct(g.row_hash.iter().copied())).sum();
    println!(
        "field census f{:03}: {} states, {} lanes, {} shapes, {} distinct rows",
        frame, state_count, lanes, groups.len(), rows_before
    );
    for group in &groups {
        println!(
            "  shape: {} lanes, {} lane-varying fields",
            group.rows,
            group.fields.len()
        );
    }

    // Every lane-varying field, in any shape.
    let labels: Vec<String> = {
        let mut set: FxHashSet<&str> = FxHashSet::default();
        for group in &groups {
            for label in group.fields.keys() {
                set.insert(label.as_str());
            }
        }
        let mut v: Vec<String> = set.into_iter().map(|s| s.to_string()).collect();
        v.sort();
        v
    };

    println!();
    println!(
        "{:<34} {:>10} {:>8} {:>14} {:>8}",
        "field", "lanes", "values", "rows_after", "after/before"
    );
    let mut measured: Vec<(String, usize, usize)> = Vec::new();
    for label in &labels {
        let mut field_lanes = 0usize;
        let mut values: FxHashSet<u64> = FxHashSet::default();
        for group in &groups {
            if let Some(keys) = group.fields.get(label) {
                field_lanes += group.rows;
                values.extend(keys.iter().copied());
            }
        }
        let after = rows_with_dropped(&groups, std::slice::from_ref(label));
        measured.push((label.clone(), values.len(), after));
        println!(
            "{:<34} {:>10} {:>8} {:>14} {:>7.1}%",
            label,
            field_lanes,
            values.len(),
            after,
            100.0 * after as f64 / rows_before.max(1) as f64
        );
    }
    measured.sort_by_key(|(_, _, after)| *after);
    println!();
    println!("best single-field collapses (rows_after / rows_before):");
    for (label, values, after) in measured.iter().take(10) {
        println!(
            "  {:<32} {:>8} values  {:>12} rows  {:>6.1}%",
            label,
            values,
            after,
            100.0 * *after as f64 / rows_before.max(1) as f64
        );
    }

    if !sets.is_empty() {
        println!();
        println!(
            "field SETS collapsed together (`field` erases it, `field:n` keeps n \
             fraction bits as CELESTE_REM_BITS does for rem):"
        );
        for set in sets {
            let after = rows_after(&groups, &set.ops);
            println!(
                "  {:<56} {:>12} rows  {:>6.1}%",
                set.name(),
                after,
                100.0 * after as f64 / rows_before.max(1) as f64
            );
        }
    }

    if let Some(path) = out_csv {
        let occupancy = per_position_rows(&groups);
        let mut per_field: Vec<BTreeMap<(i16, i16), usize>> = Vec::new();
        for label in &labels {
            per_field.push(per_position_distinct(
                &groups,
                &[(label.clone(), Collapse::Drop)],
            ));
        }
        let mut joint: Vec<BTreeMap<(i16, i16), usize>> = Vec::new();
        for set in sets {
            joint.push(per_position_distinct(&groups, &set.ops));
        }
        let mut w = std::io::BufWriter::new(std::fs::File::create(path)?);
        write!(w, "x,y,lanes,rows")?;
        for set in sets {
            write!(w, ",joint:{}", set.name())?;
        }
        for label in &labels {
            write!(w, ",{}", label)?;
        }
        writeln!(w)?;
        for (p, (l, r)) in &occupancy {
            write!(w, "{},{},{},{}", p.0, p.1, l, r)?;
            for counts in &joint {
                write!(w, ",{}", counts.get(p).copied().unwrap_or(0))?;
            }
            for counts in &per_field {
                write!(w, ",{}", counts.get(p).copied().unwrap_or(0))?;
            }
            writeln!(w)?;
        }
        w.flush()?;
        println!();
        println!(
            "per-position distinct-value counts for {} fields over {} positions -> {}",
            labels.len(),
            occupancy.len(),
            path.display()
        );
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use celeste_core::pico8_num::Pico8Num;

    /// The census's bucketing must be the SAME function the `rem` ladder
    /// applies (`abstraction::rem_bucket`, which is private), or its "what
    /// would a rung buy" numbers describe a rung nobody can build.
    #[test]
    fn bucketing_matches_the_rem_ladder() {
        for bits in [0u8, 1, 2, 4, 8, 15] {
            for raw in [0i32, 1, 0x1234, 0x1_8000, -1, -0x1_8000, -0x2_0001] {
                let n = Pico8Num::from_parts((raw >> 16) as i16, raw as u16);
                let width: i32 = 0x1_0000 >> bits;
                let want = raw.div_euclid(width) * width;
                let got = Collapse::Bits(bits).apply(n.to_bits() as u64).unwrap();
                assert_eq!(
                    got, want as u32 as u64,
                    "bits {} of raw {:#x}",
                    bits, raw
                );
            }
        }
    }

    #[test]
    fn collapse_set_parses_both_forms() {
        let set = CollapseSet::parse("player.spd.x, player.spd.y:2,player.p_jump").unwrap();
        assert_eq!(set.name(), "player.spd.x+player.spd.y:2+player.p_jump");
        assert!(matches!(set.ops[0].1, Collapse::Drop));
        assert!(matches!(set.ops[1].1, Collapse::Bits(2)));
        assert!(CollapseSet::parse("player.spd.x:16").is_err());
        assert!(CollapseSet::parse("player.spd.x:hi").is_err());
        assert!(CollapseSet::parse("").is_err());
    }
}
