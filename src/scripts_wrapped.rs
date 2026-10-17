//! Every script runs the project's binaries and cargo under `safe-run.sh`
//! (CLAUDE.md "Running safely": an unwrapped run beside a capped one once
//! took the machine down): this test reads the checked-in scripts and fails
//! on a line that runs one without `safe-run` on the same line. And check.sh
//! finds its tiers' ignored tests: every `#[ignore]` names its tier.

/// The checked-in scripts: `*.sh` and `*.py` outside `ui/` and `target/`.
fn scripts(dir: &std::path::Path, out: &mut Vec<std::path::PathBuf>) {
    for e in std::fs::read_dir(dir).expect("read_dir") {
        let p = e.expect("entry").path();
        let name = p.file_name().and_then(|n| n.to_str()).unwrap_or("");
        if p.is_dir() {
            if !name.starts_with('.') && !["target", "ui", "node_modules"].contains(&name) {
                scripts(&p, out);
            }
        } else if name.ends_with(".sh") || name.ends_with(".py") {
            out.push(p);
        }
    }
}

/// Does this (non-comment) line RUN a project binary or cargo?
fn runs_ours(line: &str, python: bool) -> bool {
    let t = line.trim_start();
    if t.starts_with('#') || t.starts_with("echo ") || t.starts_with("printf ") {
        return false;
    }
    if python {
        // A subprocess whose program is a binary of ours (by its variable,
        // or a path into target/).
        let Some(i) = t.find("subprocess.") else { return false };
        let call = &t[i..];
        let Some(j) = call.find("([") else { return false };
        let first = call[j + 2..].split([',', ']']).next().unwrap_or("").trim();
        return ["binary", "concrete", "bin", "rewrite"].contains(&first) || first.contains("target/") || first.contains("cargo");
    }
    // Shell: the binary variables of check.sh and fixtures.sh, a
    // path into target/, cargo - not where assigned or tested for (`-x`).
    let code = t.split(" #").next().unwrap_or(t);
    let invokes = |pat: &str| code.match_indices(pat).any(|(i, _)| !code[..i].ends_with("-x ") && !code[..i].ends_with('='));
    invokes("\"$B\"")
        || invokes("$B ")
        || invokes("\"$BIN\"")
        || ["cargo build", "cargo nextest", "cargo test", "cargo run", "cargo check"].iter().any(|c| code.contains(c))
        || code.split_whitespace().next().is_some_and(|w| w.contains("target/") && !w.contains('='))
}

#[test]
fn every_script_runs_our_binaries_under_safe_run() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let mut files = Vec::new();
    scripts(root, &mut files);
    files.sort();
    assert!(files.iter().any(|p| p.ends_with("tools/fixtures.sh")), "the scan found the scripts");
    let mut bad = Vec::new();
    for p in &files {
        let python = p.extension().is_some_and(|e| e == "py");
        let text = std::fs::read_to_string(p).expect("read");
        for (n, line) in text.lines().enumerate() {
            if runs_ours(line, python) && !line.contains("safe-run") {
                bad.push(format!("{}:{}: {}", p.strip_prefix(root).unwrap_or(p).display(), n + 1, line.trim()));
            }
        }
    }
    assert!(bad.is_empty(), "run under ./safe-run.sh (CLAUDE.md \"Running safely\"):\n{}", bad.join("\n"));
}

#[test]
fn the_scan_sees_an_unwrapped_run() {
    assert!(runs_ours("    \"$B\" ckhash --room 1,0 --to 44", false));
    assert!(runs_ours("        $B ckhash --room \"$ROOM\"", false));
    assert!(runs_ours("    if ! cargo build --profile quick --bins >&2; then", false));
    assert!(runs_ours("            ex = subprocess.run([binary, \"export-ui\"], cwd=M)", true));
    assert!(runs_ours("./target/quick/rewrite ckhash --to 44", false));
    assert!(!runs_ours("B=$T/quick/rewrite", false));
    assert!(!runs_ours("B=\"$repo_root/target/quick/rewrite\"", false));
    assert!(!runs_ours("if [ ! -x \"$B\" ]; then", false));
    assert!(!runs_ours("    out = subprocess.run(cmd, capture_output=True)", true));
}

/// The category runner's pure parts hold their doctests (`ui_label`: an
/// unverified result is labelled so in the web UI).
#[test]
fn the_category_runner_doctests_pass() {
    let tools = std::path::Path::new(env!("CARGO_MANIFEST_DIR")).join("tools");
    let out = std::process::Command::new("python3").args(["-m", "doctest", "category_runner.py"]).current_dir(&tools).output().expect("python3");
    assert!(out.status.success(), "tools/category_runner.py doctests:\n{}{}", String::from_utf8_lossy(&out.stdout), String::from_utf8_lossy(&out.stderr));
}

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
