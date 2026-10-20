#!/bin/bash
# THE FIXTURES of the check ladder (plans/devloop.md "The fixtures"): small
# trees at known frames under $CELESTE_FIXTURES (default
# /var/tmp/celeste-fixtures, never committed), each built by a pinned
# command and checked against its pinned fingerprint
# (gates/fixtures/NAME.ckhash: `ckhash --edges --hulls` through its last
# frame). A fixture is an INPUT: the frame checks run one frame from it.
# Fixtures are shared by every checkout and READ-ONLY once installed
# (`ensure` makes them so): a check that writes into one fails loudly.
#
#   tools/fixtures.sh ensure NAME...   build what is missing or STALE (another
#                                      spec or file format; rebuilt, loudly)
#   tools/fixtures.sh build NAME...    rebuild
#   tools/fixtures.sh pin NAME...      rebuild and PIN its fingerprint (say
#                                      why in the commit)
#   tools/fixtures.sh verify NAME...   rebuild beside it and check the pin
#                                      (./check.sh fixtures: all big)
#   tools/fixtures.sh list
#
# A level -1 fixture keeps the table it was built under (NAME/l1-table.bin,
# --l1-table): the table cache is keyed on the binary, so every rebuild of
# the binary would rebuild the table (`bench frame` refuses another). One
# built from other sources than the checkout's is reported, not rebuilt: its
# content is pinned.
# Env: BIN (default $CARGO_TARGET_DIR/quick/rewrite), CELESTE_FIXTURES.
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT=${CELESTE_FIXTURES:-/var/tmp/celeste-fixtures}
B=${BIN:-${CARGO_TARGET_DIR:-./target}/quick/rewrite}
PINS=gates/fixtures
# Every build's memory cap (safe-run.sh), as check.sh's.
MEM=20G

L1_62="--hundred --loading-jank 2 --level-minus-one 94,5"
# NAME -> FLAGS (the settings) | ROOM | COMMAND | TREE (the level dir under
# the fixture) | LAST (the tree's last frame).
spec() {
    TREE=.; FLAGS=""
    case $1 in
        # Room (1,0) 200m r0sxh, the mid frame f55 -> f56 (85k new states
        # and 125k grown in: 2.2M emissions, 2.5M edges re-pushed).
        r10) ROOM=1,0; CMD="forward --room 1,0 --level r0sxh --to 56"; LAST=56 ;;
        # Room (6,2) 100% 2300m r0sxhf under level -1 (94,5), f50 -> f51
        # (305k new states and 579k grown in: 6.6M emissions, 8.3M edges
        # re-pushed); the reference frame is r62h57.
        r62h) FLAGS=$L1_62; ROOM=6,2; CMD="forward --room 6,2 --level r0sxhf --to 51"; LAST=51 ;;
        # Room (4,2) 2100m at the object level r0sxhn under level -1 (71,5), f60 -> f61.
        r42n) FLAGS="--level-minus-one 71,5"; ROOM=4,2; CMD="forward --room 4,2 --level r0sxhn --to 61"; LAST=61 ;;
        # The same room with its objects exact (r0sxh): the reference
        # checks' object room (the reference forks r0sxhn's widened objects
        # into thousands of paths a row: plans/devloop.md).
        r42x) FLAGS="--level-minus-one 71,5"; ROOM=4,2; CMD="forward --room 4,2 --level r0sxh --to 56"; LAST=56 ;;
        # Room (6,0) 700m, platforms unknown, split frame: steps 70 -> 71.
        r60s) FLAGS="--split-frame"; ROOM=6,0; CMD="forward --room 6,0 --level r0sxhfp --to 71"; LAST=71 ;;
        # A finished search for the backward: room (1,0) r0sxh, synthetic
        # win at (40,64) (first win f47, optimum 49), horizon 56.
        r10arc) ROOM=1,0; CMD="search --room 1,0 --level r0sxh --to 56 --win-at 40,64"; TREE=level00; LAST=56 ;;
        # FLOAT twins (`--arith float`, Celia's start: plans/float.md):
        # r10's, r42n's and r42x's frames in the doubles' kernels.
        f10) FLAGS="--arith float"; ROOM=1,0; CMD="forward --room 1,0 --level r0sxh --to 56"; LAST=56 ;;
        f42n) FLAGS="--arith float --level-minus-one 71,5"; ROOM=4,2; CMD="forward --room 4,2 --level r0sxhn --to 61"; LAST=61 ;;
        f42x) FLAGS="--arith float --level-minus-one 71,5"; ROOM=4,2; CMD="forward --room 4,2 --level r0sxh --to 56"; LAST=56 ;;
        f10arc) FLAGS="--arith float"; ROOM=1,0; CMD="search --room 1,0 --level r0sxh --to 56 --win-at 40,64"; TREE=level00; LAST=56 ;;
        # BIG (a minute, ~10 GB): the reference frame (6,2) 100% f56 -> f57.
        r62h57) FLAGS=$L1_62; ROOM=6,2; CMD="forward --room 6,2 --level r0sxhf --to 57"; LAST=57 ;;
        *) echo "fixtures: no fixture $1 (tools/fixtures.sh list)" >&2; exit 2 ;;
    esac
}
ALL="r10 r62h r42n r42x r60s r10arc f10 f42n f42x f10arc"
BIG="r62h57"

# What the fixture's frames are a function of, besides its spec: the file
# formats and the order of its transfer table (a mismatch is STALE), and
# the sources (reported only). A frame check encodes the fixture's
# transfers by their ids in `xfer.bin`, so its edge bytes follow the
# table's order: a frame's new transfers are numbered at each slice's
# GATHER (the pairs first met in the slice, by content: `XferTable::merge`).
formats() {
    echo "checkpoint $(grep -o 'FORMAT_VERSION: u32 = [0-9]*' src/storage/file.rs | grep -o '[0-9]*$') edges $(grep -o '^const VERSION: u32 = [0-9]*' src/storage/edges.rs | grep -o '[0-9]*$') settings $(grep -o '^pub const VERSION: u32 = [0-9]*' src/settings.rs | grep -o '[0-9]*$') xfers gather"
}
sources() {
    { git ls-files -s src crates lua cart Cargo.lock; git diff HEAD -- src crates lua cart Cargo.lock; } | sha1sum | cut -c1-16
}

# The tree's fingerprint (as pinned; its settings its record's).
fingerprint() {
    ./safe-run.sh --memory "$MEM" -- "$B" ckhash --tree "$1/$TREE" --to "$LAST" --edges --hulls 2>/dev/null
}

# build NAME [pin|verify]: build NAME from scratch with this checkout and
# check it against its pin, then install it; `pin`: pin it instead;
# `verify`: discard it (the installed fixture stays), a missing pin fails.
build() {
    local name=$1 mode=${2:-}
    spec "$name"
    local dir=$ROOT/$name tmp=$ROOT/$name.tmp
    [[ $mode == verify ]] && tmp=$ROOT/$name.verify
    echo "[fixtures] building $name${mode:+ ($mode)}: $CMD $FLAGS" >&2
    rm -rf "$tmp"
    if [[ $mode != verify && -d $dir ]]; then
        chmod -R u+w "$dir"
        rm -rf "$dir"
    fi
    mkdir -p "$tmp"
    local t0=$SECONDS
    ./safe-run.sh --memory "$MEM" -- "$B" $CMD $FLAGS --l1-table "$tmp/l1-table.bin" --checkpoint-dir "$tmp" > "$tmp/build.out" 2> "$tmp/build.log" \
        || { echo "[fixtures] $name: the build FAILED (see $tmp/build.log)" >&2; exit 1; }
    fingerprint "$tmp" > "$tmp/ckhash.txt"
    if [[ $mode == verify && ! -f $PINS/$name.ckhash ]]; then
        echo "[fixtures] $name: no pinned fingerprint $PINS/$name.ckhash to verify against" >&2
        exit 1
    fi
    if [[ $mode == pin || ! -f $PINS/$name.ckhash ]]; then
        cp "$tmp/ckhash.txt" "$PINS/$name.ckhash"
        echo "[fixtures] $name: fingerprint PINNED to $PINS/$name.ckhash" >&2
    elif ! diff -q "$PINS/$name.ckhash" "$tmp/ckhash.txt" > /dev/null; then
        diff "$PINS/$name.ckhash" "$tmp/ckhash.txt" | head -8 >&2
        echo "[fixtures] $name: built by this checkout, it DIFFERS from its pinned fingerprint ($PINS/$name.ckhash; kept in $tmp)." >&2
        echo "[fixtures]   The forward changed. If intended: tools/fixtures.sh pin $name, and say why in the commit." >&2
        exit 1
    fi
    if [[ $mode == verify ]]; then
        rm -rf "$tmp"
        echo "[fixtures] $name: rebuilt in $((SECONDS - t0)) s, as pinned" >&2
        return
    fi
    {
        echo "name $name"
        echo "spec $FLAGS | $CMD"
        echo "formats $(formats)"
        echo "sources $(sources)"
        echo "commit $(git rev-parse --short HEAD)$(git diff --quiet HEAD -- src crates lua cart || echo +dirty)"
        echo "built $(date -Is) in $((SECONDS - t0)) s"
    } > "$tmp/manifest.txt"
    mv "$tmp" "$dir"
    chmod -R a-w "$dir"
    echo "[fixtures] $name: built in $((SECONDS - t0)) s ($(du -sh "$dir" | cut -f1))" >&2
}

# Build NAME when missing or stale; report other sources.
ensure() {
    local name=$1
    spec "$name"
    local m=$ROOT/$name/manifest.txt
    if [[ ! -f $m ]]; then
        build "$name"
        return
    fi
    local why=""
    grep -qxF "spec $FLAGS | $CMD" "$m" || why="its spec changed"
    grep -qxF "formats $(formats)" "$m" || why="${why:+$why, }the file formats changed ($(grep ^formats "$m" | cut -d' ' -f2-) -> $(formats))"
    if [[ -n $why ]]; then
        echo "[fixtures] STALE fixture $name: $why - REBUILDING" >&2
        build "$name"
        return
    fi
    if ! grep -qxF "sources $(sources)" "$m"; then
        echo "[fixtures] note: $name was built from other sources ($(grep ^commit "$m" | cut -d' ' -f2)); an input pinned by its fingerprint, not rebuilt (tools/fixtures.sh build $name)" >&2
    fi
    chmod -R a-w "$ROOT/$name"
}

case ${1:-} in
    ensure|build|pin|verify)
        verb=$1; shift
        names=""
        for w in "$@"; do
            case $w in all) names+=" $ALL" ;; big) names+=" $BIG" ;; *) names+=" $w" ;; esac
        done
        for n in $names; do
            case $verb in
                ensure) ensure "$n" ;;
                build) build "$n" ;;
                pin|verify) build "$n" "$verb" ;;
            esac
        done
        ;;
    list)
        for n in $ALL $BIG; do
            spec "$n"
            printf '%-11s %s\n' "$n" "$( [[ -f $ROOT/$n/manifest.txt ]] && grep ^built "$ROOT/$n/manifest.txt" || echo missing)"
        done
        ;;
    *) sed -n '2,/^[^#]/{/^#/p}' "$0"; exit 2 ;;
esac
