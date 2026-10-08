-- Headless driver for UniversalClassicTas: go to level UCT_LEVEL, load
-- TAS/TAS<level>.tas from the save dir (W), restart (D), clean-save (U),
-- wait for the playback to finish, report, quit.
--
-- A playback that DIES is a failure, reported as "[driver] DIED ..." and
-- quit at once. UCT itself does not stop on a death: the cart restarts the
-- room (its balloons re-drawn with rnd(1), NOT the file's seeds) and the
-- clean save keeps recording, so a later attempt that happens to exit is
-- saved as if the file had finished (gemskip-nodiag TAS30, 2026-10-08: two
-- deaths at keypress frame 29, the third attempt exited).
-- UCT_TRACE=1: one "[trace]" line per cart update of the playback (the
-- player, the balloons' offset and y), to compare with pico8_diff/replay.py.
local D = {t = 0, phase = "start", n = tonumber(os.getenv("UCT_LEVEL")), pressed = 0, waited = 0}
local function press(k) love.keypressed(k) end
local function player()
  for _, o in pairs(pico8.cart.objects) do
    if o.type == pico8.cart.player then return o end
  end
end
local function trace()
  local cart = pico8.cart
  local line = "[trace] kf " .. D.TAS.keypress_frame .. " deaths " .. cart.deaths
  local p = player()
  if p then
    line = line .. string.format(" player %s,%s rem %.6f,%.6f spd %.6f,%.6f djump %d", p.x, p.y, p.rem.x, p.rem.y, p.spd.x, p.spd.y, p.djump)
  end
  for _, o in pairs(cart.objects) do
    if o.type == cart.balloon then
      line = line .. string.format(" balloon %d,%.6f off %.6f spr %d", o.x, o.y, o.offset, o.spr)
    end
  end
  print(line)
end
function D.tick()
  D.t = D.t + 1
  if D.t < 20 then return end
  if D.phase == "start" then
    if D.pressed < D.n - 1 then
      if D.t % 3 == 0 then press("f"); D.pressed = D.pressed + 1 end
    else
      D.phase = "load"; D.mark = D.t
    end
  elseif D.phase == "load" and D.t > D.mark + 10 then
    -- UCT_DASHES: the dash count key (0-3; the gemskip categories: 1).
    if os.getenv("UCT_DASHES") then press(os.getenv("UCT_DASHES")) end
    print("[driver] at level index " .. tostring(pico8.cart.level_index() + 1) .. ", loading TAS")
    press("w"); D.phase = "restart"; D.mark = D.t
  elseif D.phase == "restart" and D.t > D.mark + 5 then
    press("d"); D.phase = "save"; D.mark = D.t
  elseif D.phase == "save" and D.t > D.mark + 5 then
    D.deaths = pico8.cart.deaths
    press("u"); D.phase = "wait"; D.mark = D.t
  elseif D.phase == "wait" then
    if os.getenv("UCT_TRACE") and D.TAS.cart_update then trace() end
    if pico8.cart.deaths ~= D.deaths then
      local p = player()
      print("[driver] DIED at keypress frame " .. D.TAS.keypress_frame .. (p and (", player " .. p.x .. "," .. p.y) or "") .. "; room " .. pico8.cart.room.x .. "," .. pico8.cart.room.y)
      love.event.quit()
      D.phase = "done"
    elseif not D.TAS.save_reproduce and not D.TAS.reproduce then
      print("[driver] playback ended at update " .. D.t .. "; room " .. pico8.cart.room.x .. "," .. pico8.cart.room.y .. " level index " .. (pico8.cart.level_index() + 1))
      love.event.quit()
      D.phase = "done"
    elseif D.t > D.mark + 3000 then
      print("[driver] TIMEOUT: playback did not end; room " .. pico8.cart.room.x .. "," .. pico8.cart.room.y)
      love.event.quit()
      D.phase = "done"
    end
  end
end
return D
