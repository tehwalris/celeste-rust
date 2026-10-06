-- Headless driver for UniversalClassicTas: go to level UCT_LEVEL, load
-- TAS/TAS<level>.tas from the save dir (W), restart (D), clean-save (U),
-- wait for the playback to finish, report, quit.
local D = {t = 0, phase = "start", n = tonumber(os.getenv("UCT_LEVEL")), pressed = 0, waited = 0}
local function press(k) love.keypressed(k) end
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
    press("u"); D.phase = "wait"; D.mark = D.t
  elseif D.phase == "wait" then
    if not D.TAS.save_reproduce and not D.TAS.reproduce then
      print("[driver] playback ended at update " .. D.t .. "; room " .. pico8.cart.room.x .. "," .. pico8.cart.room.y .. " level index " .. (pico8.cart.level_index() + 1))
      love.event.quit()
    elseif D.t > D.mark + 3000 then
      print("[driver] TIMEOUT: playback did not end; room " .. pico8.cart.room.x .. "," .. pico8.cart.room.y)
      love.event.quit()
    end
  end
end
return D
