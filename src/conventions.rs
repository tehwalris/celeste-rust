//! The repository's own conventions, checked: the Python tools' doctests
//! pass, and check.sh finds its tiers' ignored tests (every `#[ignore]`
//! names its tier). (That every run is under a memory cap is checked at run
//! time: `settings::refuse_uncapped`, one-cargo.sh.)

/// The Python tools' doctests pass: the category runner's (`ui_label`: an
/// unverified result is labelled so in the web UI; a crashed replay fails
/// the job), the PICO-8 replay's and the tools' (a crash or a missing
/// result raises, never reads as a run).
#[test]
fn the_python_doctests_pass() {
    let root = std::path::Path::new(env!("CARGO_MANIFEST_DIR"));
    let check = "import doctest, sys, celeste.pico8, celeste.love\n\
                 sys.path.insert(0, '../pico8_diff')\n\
                 import category_runner, replay\n\
                 bad = sum(doctest.testmod(m).failed for m in (celeste.pico8, celeste.love, category_runner, replay))\n\
                 sys.exit(1 if bad else 0)";
    let out = std::process::Command::new("python3").args(["-c", check]).current_dir(root.join("tools")).output().expect("python3");
    assert!(out.status.success(), "the Python doctests:\n{}{}", String::from_utf8_lossy(&out.stdout), String::from_utf8_lossy(&out.stderr));
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
