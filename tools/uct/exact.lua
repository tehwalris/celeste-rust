-- EXACT state traces from UniversalClassicTas (`rewrite validate uct --exact
-- FILE`), the float oracle's ground truth (plans/float.md).
--
-- Replaces love.run with a deterministic loop (one update and one draw per
-- iteration: UCT's own loop is paced by the wall clock and may update twice
-- per draw) and, after every cart update of the playback (the driver's
-- "wait" phase, and the update it quits on) and its draw, writes the cart's
-- state to FILE:
--   u N kf K in I        the update's index in the playback, UCT's keypress
--                        frame, the input it consumed (bits; -1: none);
--   s ID                 a sound the update played through `psfx` (1: a
--                        jump, 2: a wall jump: the jumps the jump buffer
--                        made, `golden::canonical_jumps`);
-- then its `g`, `o` and `f` lines (celeste_state.lua). `rewrite golden uct`
-- turns the traces into the goldens the oracle's tests read (gates/uct/).
local E = {}
local state = require("celeste_state")

local function bits(kp)
  local b = 0
  if kp then
    for i = 0, 5 do if kp[i] then b = b + 2 ^ i end end
  end
  return b
end

function E.install(drv)
  local file = assert(io.open(require("driver_opts").exact, "w"))
  local n = 0
  local input, kf = -1, 0
  -- The sounds of the current update (`psfx`: the original cart plays 1 on
  -- a jump, 2 on a wall jump).
  local sounds, hooked = {}, nil
  local function hook()
    local cart = pico8.cart
    if cart and cart.psfx and hooked ~= cart then
      local psfx = cart.psfx
      cart.psfx = function(id) sounds[#sounds + 1] = id; return psfx(id) end
      hooked = cart
    end
  end
  local function write_state()
    local TAS, cart = drv.TAS, pico8.cart
    if (drv.phase ~= "wait" and drv.phase ~= "done") or not TAS.cart_update then return end
    local out = {string.format("u %d kf %d in %d", n, kf, input)}
    for _, id in ipairs(sounds) do out[#out + 1] = "s " .. id end
    state.lines(out, cart)
    file:write(table.concat(out, "\n"), "\n")
    file:flush()
    n = n + 1
  end
  love.run = function()
    love.math.setRandomSeed(1)
    math.randomseed(1)
    love.load(love.arg.parseGameArguments(arg), arg)
    return function()
      love.graphics.setCanvas()
      love.event.pump()
      love.graphics.setCanvas(pico8.screen)
      for name, a, b, c, d, e, f in love.event.poll() do
        if name == "quit" then
          file:close()
          return a or 0
        end
        love.handlers[name](a, b, c, d, e, f)
      end
      local TAS = drv.TAS
      if TAS then
        kf = TAS.keypress_frame
        input = TAS.show_keys and bits(TAS.keypresses[TAS.keypress_frame]) or -1
      end
      hook()
      sounds = {}
      love.update(1 / 30)
      love.graphics.origin()
      love.draw()
      -- After the draw: the cart's frame is `_update` and `_draw` (the
      -- player's draw clamps it to the screen).
      write_state()
      flip_screen()
    end
  end
end

return E
