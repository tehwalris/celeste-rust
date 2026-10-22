-- EXACT state traces from Celia (`rewrite validate celia --exact FILE`), the
-- float oracle's second ground truth (plans/float.md), in tools/uct/
-- exact.lua's format: after every cctas step of the playback (driver.lua's
-- "wait" phase), before the clean save's seek rewinds the exit away,
--   u N kf K in I        the step's index, Celia's frame count + 1 before it,
--                        the buttons the cart saw (bits, read back from
--                        `pico8.keypressed` after the step);
-- then its `g`, `o` and `f` lines (celeste_state.lua). Celia deep-copies
-- the pico8 state every step, so nothing hooks the cart: the state is read
-- after the step.
local E = {}
local state = require("celeste_state")

function E.install(drv)
  love.math.setRandomSeed(1)
  math.randomseed(1)
  E.drv = drv
  E.file = assert(io.open(require("celia_opts").exact, "w"))
  E.n = 0
end

function E.wrap(tas)
  if E.wrapped then return end
  E.wrapped = true
  local step = tas.step
  tas.step = function(self)
    if E.drv.phase ~= "wait" then return step(self) end
    local kf = self:frame_count() + 1
    step(self)
    local b = 0
    for i = 0, 5 do if pico8.keypressed[0][i] then b = b + 2 ^ i end end
    local out = {string.format("u %d kf %d in %d", E.n, kf, b)}
    state.lines(out, pico8.cart)
    E.file:write(table.concat(out, "\n"), "\n")
    E.file:flush()
    E.n = E.n + 1
  end
end

return E
