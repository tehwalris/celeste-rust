#!/bin/bash
# Run tasdatabase-format files through UniversalClassicTas (the community's
# TAS tool) headlessly: load TAS/TAS<level>.tas (W), restart (D), clean-save
# (U = play back, trim at the level end), and print whether it finished and
# its input count. Needs: a UCT clone, a LOVE 11.x binary (the official
# AppImage, extracted, works without installing anything), and a celeste.p8
# in UCT's carts/ (the original cart's Lua + cart/ map and flags).
#
#   tools/uct/validate.sh [OPTIONS] LEVEL FILE.tas [more LEVEL FILE pairs...]
#
#   --dashes N     the dash count key (0-3; the gemskip categories: 1)
#   --capture DIR  (one LEVEL FILE pair) also write every played frame to
#                  DIR (tools/uct/capture.lua; tools/compare_video.py
#                  renders them)
#   --exact FILE   (one LEVEL FILE pair) also write the cart's exact state
#                  after every update of the playback to FILE
#                  (tools/uct/exact.lua: the float oracle's ground truth,
#                  tools/uct/golden.py)
#   --trace        print the driver's per-update "[trace]" lines (player,
#                  balloons)
#   --log          print all of LOVE's output
#   --uct DIR      the UCT clone (default ~/src/github.com/gonengazit/UniversalClassicTas)
#   --love BIN     LOVE 11.x (default /var/tmp/love/squashfs-root/AppRun)
#
# A playback that DIES before the exit is a failure ("DID NOT FINISH (died
# ...)"), whatever UCT saved afterwards (driver.lua). Each run has its own
# LOVE save identity (no clash with a concurrent run); the saved file is
# printed after "->" and also copied to the shared
# ~/.local/share/love/CelesteTAS/TAS<level>.tas. The options reach the
# game's Lua through RUN/game/driver_opts.lua, written here per pair.
set -e
# The knobs are the flags above: an environment that still sets one is refused.
if env | grep -qE '^(UCT|LOVE|UCT_[A-Z_]+)='; then
  echo "validate.sh: UCT_* / UCT / LOVE in the environment: these are flags now (see the header)" >&2; exit 2
fi
UCT=$HOME/src/github.com/gonengazit/UniversalClassicTas
LOVE=/var/tmp/love/squashfs-root/AppRun
DASHES=nil; CAPTURE=nil; EXACT=nil; TRACE=false; LOG=""
# A Lua string literal of $1.
lua_str() { printf '"%s"' "$(printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g')"; }
while [[ $# -gt 0 && $1 == --* ]]; do
  case $1 in
    --dashes) [[ ${2:-} =~ ^[0-3]$ ]] || { echo "validate.sh: --dashes 0-3, not ${2:-nothing}" >&2; exit 2; }; DASHES=$2; shift 2 ;;
    --capture) CAPTURE=$(lua_str "$2"); shift 2 ;;
    --exact) EXACT=$(lua_str "$2"); shift 2 ;;
    --trace) TRACE=true; shift ;;
    --log) LOG=1; shift ;;
    --uct) UCT=$2; shift 2 ;;
    --love) LOVE=$2; shift 2 ;;
    *) echo "validate.sh: unknown option $1 (see the header)" >&2; exit 2 ;;
  esac
done
[[ $# -ge 2 && $(( $# % 2 )) -eq 0 ]] || { echo "usage: tools/uct/validate.sh [OPTIONS] LEVEL FILE.tas [LEVEL FILE...]" >&2; exit 2; }
(( $# == 2 )) || [[ $CAPTURE == nil && $EXACT == nil ]] || { echo "validate.sh: --capture and --exact take one LEVEL FILE pair" >&2; exit 2; }
RUN=$(mktemp -d)
ID=uct-validate-$(basename "$RUN")
SAVE=$HOME/.local/share/love/$ID
LEGACY=$HOME/.local/share/love/CelesteTAS
find "$HOME/.local/share/love" -maxdepth 1 -name 'uct-validate-tmp.*' -mmin +1440 -exec rm -rf {} + 2>/dev/null || true
cp -r "$UCT/CelesteTAS" "$RUN/game"
sed -i "s/t.identity=\"CelesteTAS\"/t.identity=\"$ID\"/" "$RUN/game/conf.lua"
grep -q "\"$ID\"" "$RUN/game/conf.lua" || { echo "validate.sh: could not set the save identity" >&2; exit 1; }
cp "$(dirname "$0")/driver.lua" "$(dirname "$0")/capture.lua" "$(dirname "$0")/exact.lua" "$RUN/game/"
cat >> "$RUN/game/main.lua" <<'HOOK'

if love.filesystem.getInfo("driver_opts.lua") then
  -- A Lua error quits at once (LOVE's own handler shows it and waits).
  love.errorhandler = function(msg) print("[driver] LUA ERROR: " .. tostring(msg) .. "\n" .. debug.traceback()); os.exit(3) end
  local opts = require("driver_opts")
  local drv = require("driver")
  drv.TAS = TAS
  local orig_update = love.update
  love.update = function(dt) orig_update(dt); drv.tick() end
  if opts.capture then require("capture").install(drv) end
  if opts.exact then require("exact").install(drv) end
end
HOOK
mkdir -p "$SAVE/TAS"
while [ $# -ge 2 ]; do
  lvl=$1; f=$2; shift 2
  [[ $lvl =~ ^[0-9]+$ ]] || { echo "validate.sh: level $lvl is not a number" >&2; exit 2; }
  echo "return {level = $lvl, dashes = $DASHES, capture = $CAPTURE, exact = $EXACT, trace = $TRACE}" > "$RUN/game/driver_opts.lua"
  cp "$f" "$SAVE/TAS/TAS$lvl.tas"; rm -f "$SAVE/TAS$lvl.tas"
  log=$(cd "$RUN" && SDL_VIDEODRIVER=offscreen SDL_AUDIODRIVER=dummy ALSOFT_DRIVERS=null timeout 300 "$LOVE" game celeste.p8 2>&1 || true)
  n=$(sed 's/^[^]]*]//' "$SAVE/TAS$lvl.tas" | tr ',' '\n' | grep -c '^[0-9]' || true)
  if [ "$TRACE" = true ]; then echo "$log" | grep '^\[trace\]' || true; fi
  if [ -n "$LOG" ]; then echo "$log"; fi
  died=$(echo "$log" | grep -o 'DIED at [^;]*' | head -1 || true)
  err=$(echo "$log" | grep -o 'LUA ERROR: .*' | head -1 || true)
  # No comma inside the status: tools/celeste/love.py parses "STATUS, N inputs".
  died=${died//,/ }; err=${err//,/ }
  if [ -n "$err" ]; then st="DID NOT FINISH (${err})"
  elif [ -n "$died" ]; then st="DID NOT FINISH (${died/DIED/died})"
  elif echo "$log" | grep -q "Saved compressed"; then st="finished, clean save"
  else st="DID NOT FINISH (raw save only)"; fi
  mkdir -p "$LEGACY"; cp "$SAVE/TAS$lvl.tas" "$LEGACY/TAS$lvl.tas" 2>/dev/null || true
  echo "level $lvl $f: $st, $n inputs = $((n - 1))f -> $SAVE/TAS$lvl.tas"
done
rm -rf "$RUN"
