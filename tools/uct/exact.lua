-- EXACT state traces from UniversalClassicTas (validate.sh --exact FILE), the float
-- oracle's ground truth (plans/float.md).
--
-- Replaces love.run with a deterministic loop (one update and one draw per
-- iteration, as tools/uct/capture.lua: UCT's own loop is paced by the wall
-- clock and may update twice per draw) and, after every cart update of the
-- playback (the driver's "wait" phase, and the update it quits on) and its
-- draw, writes the cart's state to FILE:
--   u N kf K in I        the update's index in the playback, UCT's keypress
--                        frame, the input it consumed (bits; -1: none);
--   s ID                 a sound the update played through `psfx` (1: a
--                        jump, 2: a wall jump; tools/uct/canonical_jumps.py
--                        finds the jumps the jump buffer made with them);
--   g NAME VALUE         the scalar globals (`room.x`, `max_djump`, ...);
--   o INDEX TYPE         an object of `objects`, in order, then its fields:
--   f PATH VALUE         every number and boolean reachable from it (sorted,
--                        `spd.x`, `rem.y`, ...; no functions, no `type`, no
--                        `hair`), numbers as C99 hex floats (`%a`: the exact
--                        bits), booleans as true/false.
-- tools/uct/golden.py turns the traces into the goldens the oracle's tests
-- read (gates/uct/).
local E = {}

local function name_of(o)
  for k, v in pairs(pico8.cart) do
    if o.type == v and type(v) == "table" and v ~= pico8.cart.objects then return k end
  end
  return "?"
end

local function num(v)
  return string.format("%a", v)
end

local function fields(t, prefix, out, seen)
  if seen[t] then return end
  seen[t] = true
  local keys = {}
  for k, _ in pairs(t) do keys[#keys + 1] = k end
  table.sort(keys, function(a, b) return tostring(a) < tostring(b) end)
  for _, k in ipairs(keys) do
    local v = t[k]
    local path = prefix == "" and tostring(k) or (prefix .. "." .. tostring(k))
    if k ~= "type" and k ~= "hair" then
      if type(v) == "number" then
        out[#out + 1] = "f " .. path .. " " .. num(v)
      elseif type(v) == "boolean" then
        out[#out + 1] = "f " .. path .. " " .. tostring(v)
      elseif type(v) == "table" then
        fields(v, path, out, seen)
      end
    end
  end
end

local GLOBALS = {"max_djump", "freeze", "has_dashed", "has_key", "will_restart", "delay_restart", "pause_player", "deaths", "new_bg"}

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
    out[#out + 1] = "g room.x " .. num(cart.room.x)
    out[#out + 1] = "g room.y " .. num(cart.room.y)
    for _, g in ipairs(GLOBALS) do
      local v = cart[g]
      if type(v) == "number" then out[#out + 1] = "g " .. g .. " " .. num(v)
      elseif type(v) == "boolean" then out[#out + 1] = "g " .. g .. " " .. tostring(v) end
    end
    for i, o in ipairs(cart.objects) do
      out[#out + 1] = "o " .. i .. " " .. name_of(o)
      fields(o, "", out, {})
    end
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
