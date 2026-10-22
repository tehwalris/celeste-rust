-- THE CART'S STATE as the float oracle's goldens hold it (both tools'
-- exact traces, tools/uct/exact.lua and tools/celia/exact.lua; read by
-- `trace::oracle`), after an update's `u` line:
--   g NAME VALUE         the scalar globals (`room.x`, `max_djump`, ...);
--   o INDEX TYPE         an object of `objects`, in order, then its fields:
--   f PATH VALUE         every number and boolean reachable from it (sorted,
--                        `spd.x`, `rem.y`, ...; no functions, no `type`, no
--                        `hair`), numbers as C99 hex floats (`%a`: the exact
--                        bits), booleans as true/false.
local S = {}

-- An object's type's name: the cart's global holding its type table.
function S.name_of(o)
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

-- The `g`, `o` and `f` lines of `cart`, appended to `out`.
function S.lines(out, cart)
  out[#out + 1] = "g room.x " .. num(cart.room.x)
  out[#out + 1] = "g room.y " .. num(cart.room.y)
  for _, g in ipairs(GLOBALS) do
    local v = cart[g]
    if type(v) == "number" then out[#out + 1] = "g " .. g .. " " .. num(v)
    elseif type(v) == "boolean" then out[#out + 1] = "g " .. g .. " " .. tostring(v) end
  end
  for i, o in ipairs(cart.objects) do
    out[#out + 1] = "o " .. i .. " " .. S.name_of(o)
    fields(o, "", out, {})
  end
end

return S
