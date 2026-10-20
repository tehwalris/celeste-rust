#!/bin/bash
# Run tasdatabase-format files through Celia (gonengazit/Celia, cctas: the
# community's current TAS tool, which models the LOADING JANK that
# UniversalClassicTas does not) headlessly: go to the level as a user does
# (F, with Celia's default loading jank), load TAS<level>.tas (Shift+W),
# clean-save it (U), and print whether it finished and Celia's frame count.
#
#   tools/celia/validate.sh [OPTIONS] LEVEL FILE.tas [more LEVEL FILE pairs...]
#
#   --dashes N       max_djump (the gemskip categories: 1)
#   --jank-offset K  Celia's loading-jank offset (default 0: key A)
#   --fixp           Celia's --fixp: 16.16 fixed point instead of Lua doubles
#   --capture DIR    (one LEVEL FILE pair) also write every played frame to
#                    DIR (capture.lua; tools/compare_video.py --tool celia
#                    renders them)
#   --exact FILE     (one LEVEL FILE pair) also write the cart's exact state
#                    after every step of the playback to FILE (exact.lua, in
#                    tools/uct/exact.lua's format: the float oracle's second
#                    ground truth)
#   --trace          per-frame "[trace]" lines (the objects)
#   --log            all of LOVE's output
#   --celia DIR      the Celia clone (default ~/src/github.com/gonengazit/Celia)
#   --love BIN       LOVE 11.x (default /var/tmp/love/squashfs-root/AppRun)
#
# The file format is UniversalClassicTas's ("[seeds]i1,i2,...", the first
# input on the player's first frame): Celia reads and writes the same files.
# A playback that DIES before the exit is a failure (driver.lua).
# Output, one line per file (tools/celeste/love.py parses "STATUS, N"):
#   level L FILE: finished, Nf -> SAVED     or    level L FILE: DID NOT FINISH (...), 0f
# Each run has its own LOVE save identity (no clash with a concurrent run).
# The options reach the game's Lua through RUN/game/celia_opts.lua, written
# here per pair.
set -e
# The knobs are the flags above: an environment that still sets one is refused.
if env | grep -qE '^(CELIA|LOVE|CELIA_[A-Z_]+)='; then
  echo "validate.sh: CELIA_* / CELIA / LOVE in the environment: these are flags now (see the header)" >&2; exit 2
fi
CELIA=$HOME/src/github.com/gonengazit/Celia
LOVE=/var/tmp/love/squashfs-root/AppRun
DASHES=nil; JANK=nil; FIXP=""; CAPTURE=nil; EXACT=nil; TRACE=false; LOG=""
# A Lua string literal of $1.
lua_str() { printf '"%s"' "$(printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g')"; }
while [[ $# -gt 0 && $1 == --* ]]; do
  case $1 in
    --dashes) [[ ${2:-} =~ ^[0-9]$ ]] || { echo "validate.sh: --dashes N, not ${2:-nothing}" >&2; exit 2; }; DASHES=$2; shift 2 ;;
    --jank-offset) [[ ${2:-} =~ ^-?[0-9]+$ ]] || { echo "validate.sh: --jank-offset K, not ${2:-nothing}" >&2; exit 2; }; JANK=$2; shift 2 ;;
    --fixp) FIXP=--fixp; shift ;;
    --capture) CAPTURE=$(lua_str "$2"); shift 2 ;;
    --exact) EXACT=$(lua_str "$2"); shift 2 ;;
    --trace) TRACE=true; shift ;;
    --log) LOG=1; shift ;;
    --celia) CELIA=$2; shift 2 ;;
    --love) LOVE=$2; shift 2 ;;
    *) echo "validate.sh: unknown option $1 (see the header)" >&2; exit 2 ;;
  esac
done
[[ $# -ge 2 && $(( $# % 2 )) -eq 0 ]] || { echo "usage: tools/celia/validate.sh [OPTIONS] LEVEL FILE.tas [LEVEL FILE...]" >&2; exit 2; }
(( $# == 2 )) || [[ $CAPTURE == nil && $EXACT == nil ]] || { echo "validate.sh: --capture and --exact take one LEVEL FILE pair" >&2; exit 2; }
RUN=$(mktemp -d)
ID=celia-validate-$(basename "$RUN")
SAVE=$HOME/.local/share/love/$ID
find "$HOME/.local/share/love" -maxdepth 1 -name 'celia-validate-tmp.*' -mmin +1440 -exec rm -rf {} + 2>/dev/null || true
cp -r "$CELIA" "$RUN/game"
rm -rf "$RUN/game/.git"
sed -i "s/t.identity = \"Celia\"/t.identity = \"$ID\"/" "$RUN/game/conf.lua"
grep -q "\"$ID\"" "$RUN/game/conf.lua" || { echo "validate.sh: could not set the save identity" >&2; exit 1; }
cp "$(dirname "$0")/driver.lua" "$RUN/game/celia_driver.lua"
cp "$(dirname "$0")/capture.lua" "$RUN/game/celia_capture.lua"
cp "$(dirname "$0")/exact.lua" "$RUN/game/celia_exact.lua"
cat >> "$RUN/game/main.lua" <<'HOOK'

if love.filesystem.getInfo("celia_opts.lua") then
  -- A Lua error quits at once (LOVE's own handler shows it and waits).
  love.errorhandler = function(msg) print("[celia] LUA ERROR: " .. tostring(msg) .. "\n" .. debug.traceback()); os.exit(3) end
  local opts = require("celia_opts")
  local drv = require("celia_driver")
  local orig_update = love.update
  local cap = opts.capture and require("celia_capture")
  if cap then cap.install(drv) end
  local exact = opts.exact and require("celia_exact")
  if exact then exact.install(drv) end
  love.update = function(dt)
    if cap and tastool then cap.wrap(tastool) end
    if exact and tastool then exact.wrap(tastool) end
    orig_update(dt); drv.tas = tastool; drv.tick()
  end
end
HOOK
mkdir -p "$SAVE/celeste"
while [ $# -ge 2 ]; do
  lvl=$1; f=$2; shift 2
  [[ $lvl =~ ^[0-9]+$ ]] || { echo "validate.sh: level $lvl is not a number" >&2; exit 2; }
  echo "return {level = $lvl, dashes = $DASHES, jank_offset = $JANK, capture = $CAPTURE, exact = $EXACT, trace = $TRACE}" > "$RUN/game/celia_opts.lua"
  cp "$f" "$SAVE/celeste/TAS$lvl.tas"
  log=$(cd "$RUN" && SDL_VIDEODRIVER=offscreen SDL_AUDIODRIVER=dummy ALSOFT_DRIVERS=null timeout 300 "$LOVE" game $FIXP cctas celeste.p8 2>&1 || true)
  if [ "$TRACE" = true ]; then echo "$log" | grep '^\[trace\]' || true; fi
  if [ -n "$LOG" ]; then echo "$log"; fi
  echo "$log" | grep '^\[celia\] level' | sed "s|^|# |" || true
  fin=$(echo "$log" | grep -o '\[celia\] FINISHED [0-9]*f' | grep -o '[0-9]*' | head -1 || true)
  bad=$(echo "$log" | grep -o '\[celia\] \(DIED\|TIMEOUT\|ERROR\|LUA ERROR\).*' | head -1 | sed 's/^\[celia\] //' || true)
  bad=${bad//,/ }
  if [ -n "$fin" ]; then echo "level $lvl $f: finished, ${fin}f -> $SAVE/celeste/TAS$lvl.tas"
  else echo "level $lvl $f: DID NOT FINISH (${bad:-no result}), 0f"; fi
done
rm -rf "$RUN"
