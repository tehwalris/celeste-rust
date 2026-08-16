#!/usr/bin/env bash
# Differentially test our Lua interpreter's builtins and numerics against a
# real PICO-8. See README.md.
#
# Usage: pico8_diff/run.sh [case_name ...]
#   With no arguments, runs every case in cases/. With arguments, runs only
#   the named cases (with or without the .lua suffix).
#
# Environment:
#   PICO8      path to the pico8 binary (default: whatever is on PATH)
#   KEEP       set to 1 to keep the work directory even when everything passes

set -uo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
here="$repo_root/pico8_diff"
cd "$repo_root"

PICO8="${PICO8:-pico8}"
lua_run="$repo_root/target/release/lua_run"
known_fail_file="$here/known_fail.txt"

# --- preconditions, all loud -------------------------------------------------

if ! command -v "$PICO8" >/dev/null 2>&1; then
    cat >&2 <<EOF
pico8_diff: cannot find the PICO-8 binary.

  Looked for: $PICO8
  PATH:       $PATH

This harness compares our interpreter against a REAL PICO-8; without the
binary there is nothing to compare against, so it fails rather than skipping.
PICO-8 is a paid download from https://www.lexaloffle.com/pico-8.php.

Install it, then either put the binary on PATH or run:

  PICO8=/path/to/pico8 pico8_diff/run.sh
EOF
    exit 2
fi

if [ ! -x "$lua_run" ]; then
    echo "pico8_diff: building lua_run (missing at $lua_run)" >&2
    if ! cargo build --release --bin lua_run >&2; then
        echo "pico8_diff: cargo build --release --bin lua_run failed" >&2
        exit 2
    fi
fi

# --- known failures ----------------------------------------------------------
#
# A case listed here is expected to differ TODAY because of a bug we have
# already found and recorded. It is reported as XFAIL and does not turn the
# suite red - but a listed case that starts passing is reported as XPASS and
# DOES, because that means the list is stale.

declare -A known_fail_note=()
if [ -f "$known_fail_file" ]; then
    while IFS= read -r line; do
        case "$line" in ''|'#'*) continue ;; esac
        name="${line%%[[:space:]]*}"
        note="${line#"$name"}"
        note="${note#"${note%%[![:space:]]*}"}"
        known_fail_note["$name"]="$note"
    done < "$known_fail_file"
fi

# --- case selection ----------------------------------------------------------

cases=()
if [ "$#" -gt 0 ]; then
    for arg in "$@"; do
        f="$here/cases/${arg%.lua}.lua"
        if [ ! -f "$f" ]; then
            echo "pico8_diff: no such case: $f" >&2
            exit 2
        fi
        cases+=("$f")
    done
else
    while IFS= read -r f; do cases+=("$f"); done < <(find "$here/cases" -name '*.lua' | sort)
fi

if [ "${#cases[@]}" -eq 0 ]; then
    echo "pico8_diff: no cases found in $here/cases" >&2
    exit 2
fi

work="$(mktemp -d)"
fail_count=0
xpass_count=0
pass_count=0
xfail_count=0

for case_file in "${cases[@]}"; do
    name="$(basename "$case_file" .lua)"
    ours="$work/$name.ours"
    theirs="$work/$name.theirs"
    raw="$work/$name.pico8.raw"
    errs="$work/$name.ours.err"
    cart="$work/$name.p8"

    ours_status=0
    "$lua_run" "$case_file" >"$ours" 2>"$errs" || ours_status=$?

    "$here/wrap.sh" "$case_file" "$cart"
    # A cart that never returns would hang the suite; 60 s is ~1000x the time
    # any case here takes.
    timeout 60 "$PICO8" -x "$cart" >"$raw" 2>&1
    theirs_status=$?
    grep '^@P8@ ' "$raw" | sed 's/^@P8@ //' >"$theirs"

    ok=1
    reason=""
    if [ "$ours_status" -ne 0 ]; then
        ok=0
        reason="lua_run exited $ours_status"
    elif [ "$theirs_status" -ne 0 ]; then
        ok=0
        reason="pico8 exited $theirs_status"
    elif ! diff -q "$ours" "$theirs" >/dev/null; then
        ok=0
        reason="output differs"
    elif [ ! -s "$theirs" ]; then
        # Both sides empty means the cart produced nothing at all, which is
        # far more likely to be a broken cart than a case with no assertions.
        ok=0
        reason="no output from either side"
    fi

    if [ -n "${known_fail_note[$name]+x}" ]; then
        if [ "$ok" -eq 1 ]; then
            printf 'XPASS %s  (listed in known_fail.txt but now passes - update the list)\n' "$name"
            xpass_count=$((xpass_count + 1))
        else
            printf 'XFAIL %s  (%s; known: %s)\n' "$name" "$reason" "${known_fail_note[$name]}"
            xfail_count=$((xfail_count + 1))
        fi
        continue
    fi

    if [ "$ok" -eq 1 ]; then
        printf 'PASS  %s\n' "$name"
        pass_count=$((pass_count + 1))
    else
        printf 'FAIL  %s  (%s)\n' "$name" "$reason"
        fail_count=$((fail_count + 1))
        if [ -s "$errs" ]; then
            echo "--- our stderr ---"
            sed 's/^/    /' "$errs"
        fi
        if [ ! -s "$theirs" ]; then
            echo "--- raw pico8 output (no marker lines) ---"
            sed 's/^/    /' "$raw"
        fi
        echo "--- diff (ours vs pico8) ---"
        diff -u --label ours --label pico8 "$ours" "$theirs" | sed 's/^/    /'
    fi
done

printf '\n%d passed, %d failed, %d xfail, %d xpass\n' \
    "$pass_count" "$fail_count" "$xfail_count" "$xpass_count"

if [ "$fail_count" -ne 0 ] || [ "$xpass_count" -ne 0 ]; then
    echo "work directory kept: $work"
    exit 1
fi

if [ "${KEEP:-0}" = 1 ]; then
    echo "work directory kept: $work"
else
    rm -rf "$work"
fi
