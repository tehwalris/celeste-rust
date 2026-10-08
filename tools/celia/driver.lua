-- Headless driver for Celia's cctas (gonengazit/Celia, the community's
-- current TAS tool): go to level CELIA_LEVEL the way a user does (F, i.e.
-- cctas:next_level, which applies Celia's LOADING JANK: the room's objects
-- from index `prev_obj_count + offset` on get one update on the loading
-- frame, as in a real transition), load TAS<level>.tas from the save dir
-- (Shift+W), clean-save it (U: play back to the level's end, save the
-- inputs up to there), report, quit.
--
-- "[celia] FINISHED <n>f" is Celia's own count (the clean save's frames,
-- what the tasdatabase counts: inputs - 1). A death is a failure: "[celia]
-- DIED ..." (Celia keeps playing after one: the cart restarts the room).
-- Env: CELIA_DASHES (max_djump override, the gemskip categories: 1);
-- CELIA_JANK_OFFSET (Celia's loading-jank offset, default 0: key A);
-- CELIA_TRACE=1: one "[trace]" line per frame of the playback (the player
-- and every object with a phase), to compare with pico8_diff/replay.py.
local D = {t = 0, phase = "start", n = tonumber(os.getenv("CELIA_LEVEL"))}

local function name_of(o)
  for k, v in pairs(pico8.cart) do
    if o.type == v and type(v) == "table" and v ~= pico8.cart.objects then return k end
  end
  return "?"
end

local function trace()
  local cart = pico8.cart
  local line = "[trace] f " .. D.tas:frame_count() .. " deaths " .. cart.deaths
  for i, o in ipairs(cart.objects) do
    local nm = name_of(o)
    if nm ~= "room_title" and nm ~= "smoke" then
      line = line .. string.format(" %d:%s %s,%s", i, nm, tostring(o.x), tostring(o.y))
      if o.step then line = line .. string.format(" step %.4f", o.step) end
      if o.offset then line = line .. string.format(" off %.4f", o.offset) end
      if nm == "player" then line = line .. string.format(" rem %.4f,%.4f spd %.4f,%.4f", o.rem.x, o.rem.y, o.spd.x, o.spd.y) end
    end
  end
  print(line)
end

function D.tick()
  D.t = D.t + 1
  local tas = D.tas
  if D.t < 5 or tas == nil then return end
  if D.phase == "start" then
    if os.getenv("CELIA_DASHES") then tas.max_djump_overload = tonumber(os.getenv("CELIA_DASHES")) end
    for _ = 1, D.n - 1 do tas:next_level() end
    if tas:get_file_level_index() ~= D.n then
      print("[celia] ERROR: at level " .. tas:get_file_level_index() .. ", wanted " .. D.n)
      love.event.quit(); D.phase = "done"; return
    end
    if os.getenv("CELIA_JANK_OFFSET") then
      tas.loading_jank_offset = tonumber(os.getenv("CELIA_JANK_OFFSET"))
      tas:load_level(tas:level_index(), false)
    end
    local cart = pico8.cart
    print("[celia] level " .. D.n .. " room " .. cart.room.x .. "," .. cart.room.y .. " max_djump " .. pico8.cart.max_djump
      .. " jank from object " .. (tas.prev_obj_count + tas.loading_jank_offset) .. " of " .. #pico8.cart.objects)
    if tas:load_input_file() == nil then
      print("[celia] ERROR: could not load the input file"); love.event.quit(); D.phase = "done"; return
    end
    D.inputs = #tas.keystates
    D.deaths = pico8.cart.deaths
    tas:full_rewind()
    tas:begin_cleanup_save()
    D.phase = "wait"; D.mark = D.t
  elseif D.phase == "wait" then
    local cart = pico8.cart
    if os.getenv("CELIA_TRACE") then trace() end
    local p
    for _, o in pairs(cart.objects) do if o.type == cart.player then p = o end end
    if cart.deaths ~= D.deaths then
      print("[celia] DIED at frame " .. tas:frame_count() .. (p and (" player " .. p.x .. "," .. p.y) or ""))
      love.event.quit(); D.phase = "done"
    elseif tas.seek == nil then
      print("[celia] FINISHED " .. tas:frame_count() .. "f (" .. D.inputs .. " inputs in the file)")
      love.event.quit(); D.phase = "done"
    elseif tas:frame_count() > D.inputs + 60 then
      print("[celia] TIMEOUT: no exit by frame " .. tas:frame_count() .. (p and (" player " .. p.x .. "," .. p.y) or ""))
      love.event.quit(); D.phase = "done"
    end
  end
end
return D
