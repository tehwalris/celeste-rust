#!/bin/bash
# Run tasdatabase-format files through Celia (gonengazit/Celia, cctas: the
# community's current TAS tool, which models the LOADING JANK that
# UniversalClassicTas does not) headlessly: go to the level as a user does
# (F, with Celia's default loading jank), load TAS<level>.tas (Shift+W),
# clean-save it (U), and print whether it finished and Celia's frame count.
#   tools/celia/validate.sh LEVEL FILE.tas [more LEVEL FILE pairs...]
# The file format is UniversalClassicTas's ("[seeds]i1,i2,...", the first
# input on the player's first frame): Celia reads and writes the same files.
# A playback that DIES before the exit is a failure (driver.lua).
# Output, one line per file (tools/category_runner.py parses "STATUS, N"):
#   level L FILE: finished, Nf -> SAVED     or    level L FILE: DID NOT FINISH (...), 0f
# Env: CELIA_DASHES (max_djump: the gemskip categories 1); CELIA_JANK_OFFSET
#      (Celia's key-A offset, default 0); CELIA_FIXP=1 (Celia's --fixp: 16.16
#      fixed point instead of Lua doubles); CELIA_TRACE=1 (per-frame objects);
#      CELIA_LOG=1 (all of LOVE's output); CELIA (clone, default
#      ~/src/github.com/gonengazit/Celia), LOVE (default /var/tmp/love/squashfs-root/AppRun).
# CELIA_CAPTURE=DIR (one LEVEL FILE pair): also write every played frame to
# DIR (capture.lua; tools/compare_video.py --tool celia renders them).
# Each run has its own LOVE save identity (no clash with a concurrent run).
set -e
CELIA=${CELIA:-$HOME/src/github.com/gonengazit/Celia}
LOVE=${LOVE:-/var/tmp/love/squashfs-root/AppRun}
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
cat >> "$RUN/game/main.lua" <<'HOOK'

if os.getenv("CELIA_LEVEL") then
  -- A Lua error quits at once (LOVE's own handler shows it and waits).
  love.errorhandler = function(msg) print("[celia] LUA ERROR: " .. tostring(msg) .. "\n" .. debug.traceback()); os.exit(3) end
  local drv = require("celia_driver")
  local orig_update = love.update
  local cap = os.getenv("CELIA_CAPTURE") and require("celia_capture")
  if cap then cap.install(drv) end
  love.update = function(dt)
    if cap and tastool then cap.wrap(tastool) end
    orig_update(dt); drv.tas = tastool; drv.tick()
  end
end
HOOK
fixp=""
[ -n "$CELIA_FIXP" ] && fixp="--fixp"
mkdir -p "$SAVE/celeste"
while [ $# -ge 2 ]; do
  lvl=$1; f=$2; shift 2
  cp "$f" "$SAVE/celeste/TAS$lvl.tas"
  log=$(cd "$RUN" && CELIA_LEVEL=$lvl SDL_VIDEODRIVER=offscreen SDL_AUDIODRIVER=dummy ALSOFT_DRIVERS=null timeout 300 "$LOVE" game $fixp cctas celeste.p8 2>&1 || true)
  if [ -n "$CELIA_TRACE" ]; then echo "$log" | grep '^\[trace\]' || true; fi
  if [ -n "$CELIA_LOG" ]; then echo "$log"; fi
  echo "$log" | grep '^\[celia\] level' | sed "s|^|# |" || true
  fin=$(echo "$log" | grep -o '\[celia\] FINISHED [0-9]*f' | grep -o '[0-9]*' | head -1 || true)
  bad=$(echo "$log" | grep -o '\[celia\] \(DIED\|TIMEOUT\|ERROR\|LUA ERROR\).*' | head -1 | sed 's/^\[celia\] //' || true)
  bad=${bad//,/ }
  if [ -n "$fin" ]; then echo "level $lvl $f: finished, ${fin}f -> $SAVE/celeste/TAS$lvl.tas"
  else echo "level $lvl $f: DID NOT FINISH (${bad:-no result}), 0f"; fi
done
rm -rf "$RUN"
