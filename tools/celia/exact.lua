-- EXACT state traces from Celia (CELIA_EXACT=FILE), the float oracle's second
-- ground truth (plans/float.md), in tools/uct/exact.lua's
-- format: after every cctas step of the playback (driver.lua's "wait"
-- phase), before the clean save's seek rewinds the exit away,
--   u N kf K in I        the step's index, Celia's frame count + 1 before it,
--                        the buttons the cart saw (bits, read back from
--                        `pico8.keypressed` after the step);
--   g NAME VALUE         the scalar globals;
--   o INDEX TYPE / f PATH VALUE   every object and its numbers and
--                        booleans, numbers as hex floats (`%a`).
-- Celia deep-copies the pico8 state every step, so nothing hooks the cart:
-- the state is read after the step.
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

function E.install(drv)
  love.math.setRandomSeed(1)
  math.randomseed(1)
  E.drv = drv
  E.file = assert(io.open(os.getenv("CELIA_EXACT"), "w"))
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
    local cart = pico8.cart
    local b = 0
    for i = 0, 5 do if pico8.keypressed[0][i] then b = b + 2 ^ i end end
    local out = {string.format("u %d kf %d in %d", E.n, kf, b)}
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
    E.file:write(table.concat(out, "\n"), "\n")
    E.file:flush()
    E.n = E.n + 1
  end
end

return E
