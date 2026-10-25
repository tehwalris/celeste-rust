//! The repository's own conventions, checked: check.sh finds its tiers'
//! ignored tests (every `#[ignore]` names its tier), every function is
//! called, and the core names no tool. (That every run is under a memory
//! cap is checked at run time: `settings::refuse_uncapped`, one-cargo.sh.)

/// The `.rs` files under `dir` (`target/` aside).
fn sources(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    for e in std::fs::read_dir(dir).expect("read_dir") {
        let p = e.expect("entry").path();
        if p.is_dir() {
            if p.file_name().is_some_and(|n| n != "target") {
                sources(&p, out);
            }
        } else if p.extension().is_some_and(|x| x == "rs") {
            out.push(p);
        }
    }
}

/// Every `#[ignore]`d test names the tier that runs it: its reason starts
/// with `slow, ` (`./check.sh t2`), `pre-merge, ` (`./check.sh all`) or
/// `local, ` (`./check.sh local`). check.sh selects the ignored tests by
/// it; a test ignored otherwise would run in no tier.
#[test]
fn every_ignored_test_names_the_tier_that_runs_it() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    sources(&root.join("src"), &mut files);
    sources(&root.join("crates"), &mut files);
    let (mut bad, mut named) = (Vec::new(), 0);
    for p in &files {
        for (n, line) in std::fs::read_to_string(p).expect("read").lines().enumerate() {
            let t = line.trim_start();
            if !t.starts_with("#[ignore") {
                continue;
            }
            if ["slow, ", "pre-merge, ", "local, "].iter().any(|tier| t.starts_with(&format!("#[ignore = \"{tier}"))) {
                named += 1;
            } else {
                bad.push(format!("{}:{}: {t}", p.strip_prefix(root).unwrap_or(p).display(), n + 1));
            }
        }
    }
    assert!(named > 0, "the scan found the ignored tests");
    assert!(bad.is_empty(), "an #[ignore] reason starts with its tier (`slow, `, `pre-merge, ` or `local, `: plans/devloop.md):\n{}", bad.join("\n"));
}

/// A line's code: `None` for a comment line, else the line less a trailing
/// `//` comment (one outside a string literal).
fn code_of(line: &str) -> Option<&str> {
    let t = line.trim_start();
    if t.starts_with("//") {
        return None;
    }
    let (mut in_str, mut escaped) = (false, false);
    let b = line.as_bytes();
    for i in 0..b.len() {
        match b[i] {
            _ if escaped => escaped = false,
            b'\\' if in_str => escaped = true,
            b'"' => in_str = !in_str,
            b'/' if !in_str && b.get(i + 1) == Some(&b'/') => return Some(&line[..i]),
            _ => {}
        }
    }
    Some(line)
}

/// The identifiers of a code line.
fn words(code: &str) -> impl Iterator<Item = &str> {
    code.split(|c: char| !(c.is_ascii_alphanumeric() || c == '_')).filter(|w| w.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_'))
}

/// NOTHING UNCALLED: every function of the workspace (a `#[test]` aside)
/// is named on some other code line. The warning-free build cannot see
/// three kinds of dead code: a `pub` item of a library crate, a trait
/// method, and a reachable call whose callee can never fail; this sees the
/// first two. A name is matched as a word on any code line but its own
/// declarations (a trait method's every `fn` line is one), so a common
/// name (`pos`, `new`) passes on any use of the word: the check misses
/// some dead code, and never flags a called function. Trait methods called
/// only by their trait's machinery (`fmt` by `{}`, `drop`, the `full_moon`
/// visitor's `visit_*`) are named in `CALLED_BY_THEIR_TRAIT`.
#[test]
fn every_function_is_called() {
    const CALLED_BY_THEIR_TRAIT: &[&str] = &["main", "fmt", "drop", "hash", "deref", "add_assign"];
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    sources(&root.join("src"), &mut files);
    sources(&root.join("crates"), &mut files);
    // Per code line: its file and number, and the function it declares.
    let mut decls: Vec<(String, String)> = Vec::new();
    let mut named: std::collections::HashMap<String, usize> = Default::default();
    for p in &files {
        let text = std::fs::read_to_string(p).expect("read");
        let mut test_attr = false;
        for (n, line) in text.lines().enumerate() {
            let Some(code) = code_of(line) else { continue };
            let t = code.trim_start();
            // An attribute names functions too (`value_parser = parse_region`).
            if t.starts_with("#[") {
                test_attr |= t.starts_with("#[test]");
                for w in words(code) {
                    *named.entry(w.to_string()).or_default() += 1;
                }
                continue;
            }
            let mut ws = words(code).peekable();
            let mut declared = None;
            while let Some(w) = ws.next() {
                if w == "fn" {
                    declared = ws.peek().map(|n| n.to_string());
                    break;
                }
            }
            match declared {
                Some(name) => {
                    if !std::mem::take(&mut test_attr) {
                        decls.push((name.clone(), format!("{}:{}", p.strip_prefix(root).unwrap_or(p).display(), n + 1)));
                    }
                    // The declaration's other words are uses (a parameter's type).
                    for w in words(code).filter(|w| *w != name) {
                        *named.entry(w.to_string()).or_default() += 1;
                    }
                }
                None => {
                    test_attr = false;
                    for w in words(code) {
                        *named.entry(w.to_string()).or_default() += 1;
                    }
                }
            }
        }
    }
    assert!(decls.len() > 1000, "the scan found the functions ({})", decls.len());
    let uncalled: Vec<String> = decls
        .iter()
        .filter(|(name, _)| !named.contains_key(name) && !CALLED_BY_THEIR_TRAIT.contains(&name.as_str()) && !name.starts_with("visit_"))
        .map(|(name, at)| format!("{at}: fn {name}"))
        .collect();
    assert!(uncalled.is_empty(), "functions no code line names (delete them, or name a trait's in CALLED_BY_THEIR_TRAIT):\n{}", uncalled.join("\n"));
}

/// The lines of `text` that are not test code, numbered: a `#[cfg(test)]`
/// item (a `mod tests { .. }`, a `use ..;`) is skipped to its end by its
/// braces, comments by `code_of`.
fn non_test_lines(text: &str) -> Vec<(usize, &str)> {
    let (mut out, mut skip, mut depth, mut pending) = (Vec::new(), false, 0i64, false);
    for (n, line) in text.lines().enumerate() {
        let Some(code) = code_of(line) else { continue };
        let t = code.trim();
        if !skip && t.starts_with("#[cfg(test)]") {
            pending = true;
            continue;
        }
        if pending && t.starts_with("#[") {
            continue;
        }
        if pending {
            (pending, skip, depth) = (false, true, 0);
        }
        if skip {
            depth += code.matches('{').count() as i64 - code.matches('}').count() as i64;
            if depth <= 0 && (t.ends_with('}') || t.ends_with(';')) {
                skip = false;
            }
            continue;
        }
        out.push((n + 1, code));
    }
    out
}

/// THE CORE NAMES NO TOOL (plans/architecture.md "Code layout"): no file
/// outside `src/check/`, `src/ui_export.rs` and `src/bin/` names
/// `crate::check` or `crate::ui_export` in code that is not a test (a test
/// may hold the core against a tool: `trace::oracle`), nor does a test-only
/// module file count; the tools are the core's users, never the reverse.
#[test]
fn the_core_names_no_tool() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    sources(&root.join("src"), &mut files);
    sources(&root.join("crates"), &mut files);
    // The module files declared `#[cfg(test)] mod NAME;` (test-only).
    let mut test_only: Vec<std::path::PathBuf> = Vec::new();
    for p in &files {
        let text = std::fs::read_to_string(p).expect("read");
        let lines: Vec<&str> = text.lines().map(str::trim).collect();
        for w in lines.windows(2) {
            if let (true, Some(name)) = (w[0] == "#[cfg(test)]", w[1].strip_prefix("mod ").and_then(|m| m.strip_suffix(';'))) {
                let dir = if p.file_name().is_some_and(|f| f == "mod.rs" || f == "lib.rs") { p.parent().expect("a dir").to_path_buf() } else { p.with_extension("") };
                test_only.extend([dir.join(format!("{name}.rs")), dir.join(name).join("mod.rs")]);
            }
        }
    }
    let tools = ["src/check/", "src/ui_export.rs", "src/bin/"];
    let (mut bad, mut users) = (Vec::new(), 0);
    for p in &files {
        let rel = p.strip_prefix(root).unwrap_or(p).to_string_lossy().into_owned();
        let text = std::fs::read_to_string(p).expect("read");
        for (n, code) in non_test_lines(&text) {
            let names = ["crate::check", "crate::ui_export", "celeste_rust::check", "celeste_rust::ui_export"].iter().any(|t| code.contains(t));
            if !names {
                continue;
            }
            if tools.iter().any(|t| rel.starts_with(t)) {
                users += 1;
            } else if !test_only.contains(p) {
                bad.push(format!("{rel}:{n}: {}", code.trim()));
            }
        }
    }
    assert!(users > 0, "the scan found the tools' users");
    assert!(bad.is_empty(), "the core names a tool (the tools use the core, never the reverse):\n{}", bad.join("\n"));
}
