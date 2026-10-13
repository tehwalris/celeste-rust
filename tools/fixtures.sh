#!/bin/bash
# THE FIXTURES of the fast check ladder (plans/devloop.md): small trees at
# known frames under $CELESTE_FIXTURES (default /var/tmp/celeste-fixtures,
# never committed), each built once by a pinned command and checked against
# its pinned fingerprint (gates/fixtures/NAME.ckhash: `ckhash --edges
# --hulls` through its last frame: its frames, edges and hulls). A fixture is an
# INPUT: the single-frame checks run one frame from it with the code being
# tested and compare that frame's output with gates/fixtures/NAME.frame.
#
#   tools/fixtures.sh ensure NAME...   build what is missing or STALE (another
#                                      spec or file format; rebuilt, loudly)
#   tools/fixtures.sh build NAME...    rebuild
#   tools/fixtures.sh pin NAME...      rebuild and PIN its fingerprint (say
#                                      why in the commit)
#   tools/fixtures.sh list
#
# A fixture under level -1 keeps the table it was built under
# (NAME/l1-table.bin, --l1-table): the table cache is keyed on the
# binary, so without it every rebuild of the binary would rebuild the table
# (12-60 s) before the frame; the tree records the table's fingerprint and
# `bench-frame` refuses another.
#
# A fixture built from other sources than the checkout's is reported, not
# rebuilt: its content is pinned by its fingerprint, and a code change that
# moves a frame shows in the frame checks (and in t2's full-room gates).
# Env: BIN (default $CARGO_TARGET_DIR/quick/rewrite), CELESTE_FIXTURES.
set -euo pipefail
cd "$(dirname "$0")/.."
ROOT=${CELESTE_FIXTURES:-/var/tmp/celeste-fixtures}
B=${BIN:-${CARGO_TARGET_DIR:-./target}/quick/rewrite}
PINS=gates/fixtures

L1_62="--hundred --loading-jank 2 --level-minus-one 94,5"
# NAME -> FLAGS (the settings) | ROOM | COMMAND | TREE (the level dir under the fixture) | LAST
# frame (the tree's) | DEPENDS (a capture's tree fixture).
spec() {
    DEP=""; TREE=.; FLAGS=""
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
        # The storage replay's capture: r10's f56 emissions (bench-storage).
        cap-r10) DEP=r10; ROOM=1,0; CMD="capture 56"; LAST=56 ;;
        # BIG (minutes, ~10 GB): the reference frame (6,2) 100% f56 -> f57,
        # and its 12 GB capture.
        r62h57) FLAGS=$L1_62; ROOM=6,2; CMD="forward --room 6,2 --level r0sxhf --to 57"; LAST=57 ;;
        cap-r62h57) DEP=r62h57; FLAGS=$L1_62; ROOM=6,2; CMD="capture 57"; LAST=57 ;;
        *) echo "fixtures: no fixture $1 (tools/fixtures.sh list)" >&2; exit 2 ;;
    esac
}
ALL="r10 r62h r42n r42x r60s r10arc cap-r10"
BIG="r62h57 cap-r62h57"

# What the fixture's frames are a function of, besides its spec: the file
# formats and the order of its transfer table (a mismatch is STALE), and
# the sources (reported only). A frame check encodes the fixture's
# transfers by their ids in `xfer.bin`, so its edge bytes follow the
# table's order: since 2026-10-12 (branch emit-batch) a frame's new
# transfers are numbered CANONICALLY (by their edges in the frame, then
# content: `edges::XferTable::canonical`), the table a function of the
# search; before, in their discovery order, which the wave's slices set (a
# fixture cut by another slice size passed its fingerprint and failed the
# metrics).
formats() {
    echo "checkpoint $(grep -o 'FORMAT_VERSION: u32 = [0-9]*' src/search/checkpoint.rs | grep -o '[0-9]*$') edges $(grep -o '^const VERSION: u32 = [0-9]*' src/storage/edges.rs | grep -o '[0-9]*$') settings $(grep -o '^pub const VERSION: u32 = [0-9]*' src/settings.rs | grep -o '[0-9]*$') xfers canonical"
}
sources() {
    { git ls-files -s src crates lua cart Cargo.lock; git diff HEAD -- src crates lua cart Cargo.lock; } | sha1sum | cut -c1-16
}

# The tree's fingerprint (as pinned).
fingerprint() {
    local dir=$1/$TREE
    ./safe-run.sh -- "$B" ckhash --room "$ROOM" --to "$LAST" --edges --hulls --checkpoint-dir "$dir" 2>/dev/null
}

build() {
    local name=$1 pin=${2:-}
    spec "$name"
    local dir=$ROOT/$name tmp=$ROOT/$name.tmp
    [[ -n $DEP ]] && ensure "$DEP" && spec "$name"
    echo "[fixtures] building $name: $CMD $FLAGS" >&2
    rm -rf "$tmp" "$dir"
    mkdir -p "$tmp"
    local t0=$SECONDS
    if [[ $CMD == capture* ]]; then
        # One wave of the tree fixture, captured (--emit-capture).
        local f=${CMD#capture }
        ./safe-run.sh -- "$B" bench-frame --level-dir "$ROOT/$DEP" --frame $((f - 1)) --edges --room "$ROOM" --level "$(level_of "$DEP")" $FLAGS --l1-table "$ROOT/$DEP/l1-table.bin" --emit-capture "$tmp" > "$tmp/build.out" 2> "$tmp/build.log" \
            || { echo "[fixtures] $name: the capture FAILED (see $tmp/build.log)" >&2; exit 1; }
        # The captured wave is the tree fixture's frame check: the same lines.
        if [[ -z $pin && -f $PINS/$DEP.frame ]] && ! diff -q "$PINS/$DEP.frame" "$tmp/build.out" > /dev/null; then
            echo "[fixtures] $name: the captured wave's f/e lines differ from $PINS/$DEP.frame" >&2
            exit 1
        fi
    else
        ./safe-run.sh -- "$B" $CMD $FLAGS --l1-table "$tmp/l1-table.bin" --checkpoint-dir "$tmp" > "$tmp/build.out" 2> "$tmp/build.log" \
            || { echo "[fixtures] $name: the build FAILED (see $tmp/build.log)" >&2; exit 1; }
        fingerprint "$tmp" > "$tmp/ckhash.txt"
        if [[ -n $pin || ! -f $PINS/$name.ckhash ]]; then
            cp "$tmp/ckhash.txt" "$PINS/$name.ckhash"
            echo "[fixtures] $name: fingerprint PINNED to $PINS/$name.ckhash" >&2
        elif ! diff -q "$PINS/$name.ckhash" "$tmp/ckhash.txt" > /dev/null; then
            diff "$PINS/$name.ckhash" "$tmp/ckhash.txt" | head -8 >&2
            echo "[fixtures] $name: built by this checkout, it DIFFERS from its pinned fingerprint ($PINS/$name.ckhash; kept in $tmp)." >&2
            echo "[fixtures]   The forward changed. If intended: tools/fixtures.sh pin $name, and say why in the commit." >&2
            exit 1
        fi
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
    echo "[fixtures] $name: built in $((SECONDS - t0)) s ($(du -sh "$dir" | cut -f1))" >&2
}

level_of() {
    spec "$1"
    echo "$CMD" | grep -o -- '--level [a-z0-9]*' | cut -d' ' -f2
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
}

case ${1:-} in
    ensure|build|pin)
        verb=$1; shift
        names=$*
        [[ $names == all ]] && names=$ALL
        [[ $names == big ]] && names=$BIG
        for n in $names; do
            case $verb in
                ensure) ensure "$n" ;;
                build) build "$n" ;;
                pin) build "$n" pin ;;
            esac
        done
        ;;
    list)
        for n in $ALL $BIG; do
            spec "$n"
            printf '%-11s %s\n' "$n" "$( [[ -f $ROOT/$n/manifest.txt ]] && grep ^built "$ROOT/$n/manifest.txt" || echo missing)"
        done
        ;;
    *) sed -n '2,22p' "$0"; exit 2 ;;
esac
