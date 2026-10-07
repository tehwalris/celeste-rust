#!/bin/bash
# Run tasdatabase-format files through UniversalClassicTas (the community's
# TAS tool) headlessly: load TAS/TAS<level>.tas (W), restart (D), clean-save
# (U = play back, trim at the level end), and print whether it finished and
# its input count. Needs: a UCT clone, a LOVE 11.x binary (the official
# AppImage, extracted, works without installing anything), and a celeste.p8
# in UCT's carts/ (the original cart's Lua + cart/ map and flags).
#   tools/uct/validate.sh LEVEL FILE.tas [more LEVEL FILE pairs...]
# UCT_CAPTURE=DIR (one LEVEL FILE pair): also write every played frame to DIR
# (tools/uct/capture.lua; tools/compare_video.py renders them).
# Env: UCT_DASHES (dash count key, gemskip: 1); UCT (clone, default ~/src/github.com/gonengazit/UniversalClassicTas),
#      LOVE (default /var/tmp/love/squashfs-root/AppRun).
set -e
UCT=${UCT:-$HOME/src/github.com/gonengazit/UniversalClassicTas}
LOVE=${LOVE:-/var/tmp/love/squashfs-root/AppRun}
SAVE=$HOME/.local/share/love/CelesteTAS
RUN=$(mktemp -d)
cp -r "$UCT/CelesteTAS" "$RUN/game"
cp "$(dirname "$0")/driver.lua" "$(dirname "$0")/capture.lua" "$RUN/game/"
cat >> "$RUN/game/main.lua" <<'HOOK'

if os.getenv("UCT_LEVEL") then
  local drv = require("driver")
  drv.TAS = TAS
  local orig_update = love.update
  love.update = function(dt) orig_update(dt); drv.tick() end
  if os.getenv("UCT_CAPTURE") then require("capture").install(drv) end
end
HOOK
mkdir -p "$SAVE/TAS"
while [ $# -ge 2 ]; do
  lvl=$1; f=$2; shift 2
  cp "$f" "$SAVE/TAS/TAS$lvl.tas"; rm -f "$SAVE/TAS$lvl.tas"
  log=$(cd "$RUN" && UCT_LEVEL=$lvl SDL_VIDEODRIVER=offscreen SDL_AUDIODRIVER=dummy ALSOFT_DRIVERS=null timeout 300 "$LOVE" game celeste.p8 2>&1 || true)
  n=$(sed 's/^[^]]*]//' "$SAVE/TAS$lvl.tas" | tr ',' '\n' | grep -c '^[0-9]' || true)
  if echo "$log" | grep -q "Saved compressed"; then st="finished, clean save"; else st="DID NOT FINISH (raw save only)"; fi
  echo "level $lvl $f: $st, $n inputs = $((n - 1))f -> $SAVE/TAS$lvl.tas"
done
rm -rf "$RUN"
