//! EVERY `CELESTE_*` ENVIRONMENT KNOB, declared once: its kind, default,
//! role and meaning. Code reads a knob only through `flag`, `value` or
//! `number` here, which refuse a name not declared below; a binary starts
//! with `startup`, which refuses an undeclared `CELESTE_*` variable in the
//! environment (a typo) and a malformed flag, and logs the knobs set.
//!
//! A FLAG is `1` (on) or `0` (off); unset is off; anything else is an error
//! (`CELESTE_KERNEL_KEY_CHECK=0` used to turn the check ON). A VALUE is
//! parsed where it is read, with a loud error on a bad one.
//!
//! The knobs that change what a tree holds (role `Search`) are recorded in
//! the tree (`frame::TreeSettings`) and a tree built under other values is
//! refused; level -1's `LEVEL_MINUS_ONE` is recorded by its own file
//! (`frame::TreeFilter`: a raise changes it).

/// What a knob is for.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Role {
    /// Changes the game or the search space: recorded in a tree.
    Search,
    /// Changes only the concrete phase or the level -1 filter (recorded
    /// otherwise, or not part of a tree).
    Run,
    /// Resources and scheduling: never changes a result.
    Resource,
    /// Logging, dumps, self-checks: never changes a result.
    Diagnostic,
    /// Read by the shell tools only (`tools/fixtures.sh`, `check.sh`), never
    /// by a binary; declared so the binaries accept it in the environment.
    Script,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Kind {
    /// `1` or `0`.
    Flag,
    /// A string parsed where it is read; `default` documents the value used
    /// when unset.
    Value,
}

pub struct Knob {
    pub name: &'static str,
    pub kind: Kind,
    pub role: Role,
    /// The behaviour when unset ("" for none).
    pub default: &'static str,
    pub doc: &'static str,
}

const fn flag_knob(name: &'static str, role: Role, doc: &'static str) -> Knob {
    Knob { name, kind: Kind::Flag, role, default: "0", doc }
}

const fn value_knob(name: &'static str, role: Role, default: &'static str, doc: &'static str) -> Knob {
    Knob { name, kind: Kind::Value, role, default, doc }
}

/// The knobs. Keep this list short: a new knob needs a reason a CLI flag or
/// a constant would not do.
pub const KNOBS: &[Knob] = &[
    // The game and the search space (recorded in a tree).
    value_knob("CELESTE_START_ROOM", Role::Search, "1,0", "the room \"x,y\" (set by `--room`)"),
    value_knob("CELESTE_WIN_AT_XY", Role::Search, "", "a synthetic win \"x,y\" or \"x0..x1,y0..y1\" (set by `--win-at`)"),
    flag_knob("CELESTE_SPLIT_FRAME", Role::Search, "two search steps a frame (the split cart)"),
    value_knob("CELESTE_REGION", Role::Search, "16,6", "the kernels' region grid \"px,S\" or `off`"),
    value_knob("CELESTE_STORAGE_REGION", Role::Search, "8", "the storage region side, 8 or 16"),
    flag_knob("CELESTE_HUNDRED", Role::Search, "100%: a win needs the room's berry"),
    flag_knob("CELESTE_NODIAG", Role::Search, "no diagonal dash starts"),
    flag_knob("CELESTE_ONLYDIAG", Role::Search, "only diagonal dash starts"),
    flag_knob("CELESTE_GEMSKIP", Role::Search, "the orb room won without the orb; one dash after it"),
    value_knob("CELESTE_LOADING_JANK", Role::Search, "none", "the loading frame from object J (real play's room entry), or `none`"),
    value_knob("CELESTE_BALLOON_SEEDS", Role::Search, "", "the balloons' phases fixed, \"s1,s2,..\""),
        // The run around the tree.
    value_knob("CELESTE_LEVEL_MINUS_ONE", Role::Run, "", "the level -1 filter \"H,S\" (H in steps; the tree records it)"),
    value_knob("CELESTE_CONCRETE_BALLOON_SEEDS", Role::Run, "", "the balloons' phases for the concrete phase only"),
    // Resources.
    value_knob("CELESTE_THREADS", Role::Resource, "physical cores", "worker threads"),
    value_knob("CELESTE_MIN_FREE_GB", Role::Resource, "20", "stop the forward below this much free disk (plus 3x the last frame)"),
    flag_knob("CELESTE_TRIM_ROWS", Role::Resource, "old frames keep keys, cells and wins only (the files say so)"),
    value_knob("CELESTE_L1_TABLE", Role::Resource, "", "the level -1 table pinned by its FILE (the fixtures), instead of the cache"),
    // Diagnostics.
    flag_knob("CELESTE_KERNEL_KEY_CHECK", Role::Diagnostic, "every emitted row's key recomputed by the boundary; every unit's copy of a new state compared"),
    flag_knob("CELESTE_KERNEL_EXPLAIN", Role::Diagnostic, "kernels keep their graphs: a decline names its premise"),
    value_knob("CELESTE_KERNEL_MIX", Role::Diagnostic, "", "DIR: the instruction-mix report per kernel (keeps the .so files)"),
    flag_knob("CELESTE_KERNEL_METRICS", Role::Diagnostic, "the kernels' census and slice counters (`bench-frame --metrics`)"),
    flag_knob("CELESTE_BUILD_TRACE", Role::Diagnostic, "the kernel build's and the lattice walk's progress on stderr"),
    flag_knob("CELESTE_PHASES", Role::Diagnostic, "the wave workers' phase timings"),
    value_knob("CELESTE_EMIT_CAPTURE", Role::Diagnostic, "", "DIR: capture the wave's emissions (`bench-storage`)"),
    value_knob("CELESTE_EMIT_CAPTURE_FRAME", Role::Diagnostic, "every frame", "the frame CELESTE_EMIT_CAPTURE captures"),
    value_knob("CELESTE_WALK_REGIONS", Role::Diagnostic, "", "the lattice walk traces only these regions \"(ix,iy) ..\" (a probe)"),
    value_knob("CELESTE_L1_PATH", Role::Diagnostic, "", "the level -1 probe prints the table's fastest path from \"x,y\""),
    // The shell tools'.
    value_knob("CELESTE_FIXTURES", Role::Script, "/var/tmp/celeste-fixtures", "where tools/fixtures.sh keeps the fixtures"),
];

fn knob(name: &str) -> &'static Knob {
    KNOBS.iter().find(|k| k.name == name).unwrap_or_else(|| panic!("{name}: not a declared knob (src/knobs.rs)"))
}

/// A flag's value, or why it is not one.
fn parse_flag(name: &str, v: &str) -> Result<bool, String> {
    match v {
        "1" => Ok(true),
        "0" => Ok(false),
        _ => Err(format!("{name}={v:?}: a flag is 1 or 0")),
    }
}

fn raw(name: &str) -> Option<String> {
    match std::env::var(name) {
        Ok(v) => Some(v),
        Err(std::env::VarError::NotPresent) => None,
        Err(std::env::VarError::NotUnicode(v)) => panic!("{name}={v:?}: not UTF-8"),
    }
}

/// A flag knob: unset or `0` is off, `1` on; anything else panics.
pub fn flag(name: &str) -> bool {
    let k = knob(name);
    assert_eq!(k.kind, Kind::Flag, "{name} is not a flag");
    raw(name).is_some_and(|v| parse_flag(name, &v).unwrap_or_else(|e| panic!("{e}")))
}

/// A value knob, if set (an empty value panics: unset it instead).
pub fn value(name: &str) -> Option<String> {
    let k = knob(name);
    assert_eq!(k.kind, Kind::Value, "{name} is a flag (knobs::flag)");
    let v = raw(name)?;
    assert!(!v.trim().is_empty(), "{name} is set but empty: unset it");
    Some(v)
}

/// A value knob parsed as a number, if set; a malformed one panics.
pub fn number<T: std::str::FromStr>(name: &str) -> Option<T> {
    value(name).map(|v| v.trim().parse().unwrap_or_else(|_| panic!("{name}={v:?}: not a number")))
}

/// Every `CELESTE_*` variable in the environment: declared, and a flag's
/// value 1 or 0. All problems at once.
pub fn check_env() -> anyhow::Result<()> {
    let mut bad = Vec::new();
    for (k, v) in std::env::vars_os() {
        let Some(k) = k.to_str().filter(|k| k.starts_with("CELESTE_")) else { continue };
        let Some(decl) = KNOBS.iter().find(|d| d.name == k) else {
            bad.push(format!("{k}: no such knob (a typo? the knobs: src/knobs.rs)"));
            continue;
        };
        let Some(v) = v.to_str() else {
            bad.push(format!("{k}: not UTF-8"));
            continue;
        };
        match decl.kind {
            Kind::Flag => {
                if let Err(e) = parse_flag(k, v) {
                    bad.push(e);
                }
            }
            Kind::Value if v.trim().is_empty() => bad.push(format!("{k} is set but empty: unset it")),
            Kind::Value => {}
        }
    }
    bad.sort();
    anyhow::ensure!(bad.is_empty(), "the environment's CELESTE_* knobs:\n  {}", bad.join("\n  "));
    Ok(())
}

/// A binary's first call: `check_env`, then one `[knobs]` line on stderr
/// with every knob set to other than its default (the run's log says what
/// it ran under).
pub fn startup() -> anyhow::Result<()> {
    check_env()?;
    let set: Vec<String> = KNOBS
        .iter()
        .filter(|k| k.role != Role::Script)
        .filter_map(|k| raw(k.name).filter(|v| v != k.default).map(|v| format!("{}={v}", k.name)))
        .collect();
    if !set.is_empty() {
        eprintln!("[knobs] {}", set.join(" "));
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Strict flags; unknown names refused; a declared name per kind.
    #[test]
    fn flags_are_one_or_zero_and_unknown_knobs_are_refused() {
        assert_eq!(parse_flag("X", "1"), Ok(true));
        assert_eq!(parse_flag("X", "0"), Ok(false));
        for v in ["", "true", "yes", "2", " 1"] {
            assert!(parse_flag("X", v).is_err(), "{v:?}");
        }
        // nextest: this process alone.
        std::env::set_var("CELESTE_KERNEL_KEY_CHECK", "0");
        assert!(!flag("CELESTE_KERNEL_KEY_CHECK"), "0 is off");
        std::env::set_var("CELESTE_KERNEL_KEY_CHECK", "1");
        assert!(flag("CELESTE_KERNEL_KEY_CHECK"));
        assert!(check_env().is_ok());
        std::env::set_var("CELESTE_KERNEL_KEY_CHECK", "on");
        assert!(check_env().is_err(), "a malformed flag");
        std::env::remove_var("CELESTE_KERNEL_KEY_CHECK");
        std::env::set_var("CELESTE_SPLIT_FRAMES", "1");
        let e = check_env().expect_err("a typo");
        assert!(e.to_string().contains("CELESTE_SPLIT_FRAMES: no such knob"), "{e}");
        std::env::remove_var("CELESTE_SPLIT_FRAMES");
        let mut names: Vec<&str> = KNOBS.iter().map(|k| k.name).collect();
        names.sort();
        names.dedup();
        assert_eq!(names.len(), KNOBS.len(), "a knob declared twice");
        assert!(KNOBS.iter().all(|k| k.name.starts_with("CELESTE_")));
    }
}
