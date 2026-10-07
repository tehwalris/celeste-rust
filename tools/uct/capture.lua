-- Frame capture for the headless UniversalClassicTas driver (UCT_CAPTURE=DIR).
--
-- Replaces love.run with a deterministic loop: exactly one update and one
-- draw per iteration (UCT's own loop is paced by the wall clock, so it may
-- update twice per draw), fixed random seeds, no audio. During the playback
-- (the driver's "wait" phase) every frame the cart updated is written to DIR:
--   frames.bin  per frame 3 x 128x128 RGBA8 layers: the screen (the palette
--               INDEX is in red, index = round(r * 15 / 255); UCT's shaders
--               work in indices), the player layer (player or player_spawn
--               with its hair, alpha 0 elsewhere), the smoke layer;
--   frames.jsonl per frame: the input the update consumed, UCT's timer, the
--               player, and whether the room changed during it (`exit`).
-- The player and smoke layers come from drawing those objects a second time
-- into their own canvas, the hair saved and restored around it (draw_hair
-- moves the hair), so the game's own frame is unchanged.
-- UCT's overlay (timer, input display) is turned off: the compositor draws its own.
local C = {}

local function hair_save(o)
  if not o.hair then return nil end
  local s = {}
  for i, h in ipairs(o.hair) do s[i] = {h.x, h.y} end
  return s
end

local function hair_restore(o, s)
  if not s then return end
  for i, h in ipairs(o.hair) do h.x, h.y = s[i][1], s[i][2] end
end

function C.setup(drv)
  local cart = pico8.cart
  C.drv = drv
  C.dir = os.getenv("UCT_CAPTURE")
  C.bin = assert(io.open(C.dir .. "/frames.bin", "wb"))
  C.meta = assert(io.open(C.dir .. "/frames.jsonl", "w"))
  C.P = love.graphics.newCanvas(128, 128)
  C.S = love.graphics.newCanvas(128, 128)
  C.n = 0
  local draw_object = cart.draw_object
  cart.draw_object = function(o)
    local t = o.type
    local layer = (t == cart.player or t == cart.player_spawn) and C.P or (t == cart.smoke and C.S) or nil
    if layer then
      local s = hair_save(o)
      love.graphics.setCanvas(layer)
      draw_object(o)
      love.graphics.setCanvas(pico8.screen)
      hair_restore(o, s)
    end
    draw_object(o)
  end
  local update = cart._update
  cart._update = function()
    local rx, ry = cart.room.x, cart.room.y
    update()
    if cart.room.x ~= rx or cart.room.y ~= ry then C.exited = true end
  end
end

local function bits(kp)
  local b = 0
  if kp then
    for i = 0, 5 do if kp[i] then b = b + 2 ^ i end end
  end
  return b
end

function C.pre_update()
  local TAS = C.drv.TAS
  C.exited = false
  C.input = TAS.show_keys and bits(TAS.keypresses[TAS.keypress_frame]) or -1
  C.kf = TAS.keypress_frame
end

function C.pre_draw()
  C.drv.TAS.showdebug = false
  -- A freeze frame draws nothing (`_draw` returns early): keep the layers too.
  if (pico8.cart.freeze or 0) <= 0 then
    for _, c in ipairs({C.P, C.S}) do
      love.graphics.setCanvas(c)
      love.graphics.clear(0, 0, 0, 0)
    end
    love.graphics.setCanvas(pico8.screen)
  end
end

function C.post_draw()
  local TAS = C.drv.TAS
  if C.drv.phase ~= "wait" or not TAS.cart_update then return end
  love.graphics.setCanvas()
  for _, c in ipairs({pico8.screen, C.P, C.S}) do
    C.bin:write(c:newImageData():getString())
  end
  love.graphics.setCanvas(pico8.screen)
  local cart, px, py = pico8.cart, "null", "null"
  for _, o in pairs(cart.objects) do
    if o.type == cart.player then px, py = o.x, o.y end
  end
  C.meta:write(string.format(
    '{"n":%d,"input":%d,"keypress_frame":%d,"practice_time":%d,"exit":%s,"freeze":%d,"room":[%d,%d],"player":[%s,%s]}\n',
    C.n, C.input, C.kf, TAS.practice_time, tostring(C.exited), cart.freeze or 0, cart.room.x, cart.room.y, px, py))
  C.n = C.n + 1
  if C.exited then C.bin:flush(); C.meta:flush() end
end

function C.install(drv)
  love.run = function()
    love.math.setRandomSeed(1)
    math.randomseed(1)
    love.load(love.arg.parseGameArguments(arg), arg)
    C.setup(drv)
    return function()
      love.graphics.setCanvas()
      love.event.pump()
      love.graphics.setCanvas(pico8.screen)
      for name, a, b, c, d, e, f in love.event.poll() do
        if name == "quit" then
          C.bin:close(); C.meta:close()
          return a or 0
        end
        love.handlers[name](a, b, c, d, e, f)
      end
      C.pre_update()
      love.update(1 / 30)
      C.pre_draw()
      love.graphics.origin()
      love.draw()
      C.post_draw()
      flip_screen()
    end
  end
end

return C
