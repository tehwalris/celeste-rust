-- Frame capture for the headless Celia driver (CELIA_CAPTURE=DIR), the
-- counterpart of tools/uct/capture.lua with the same output format, so
-- tools/compare_video.py (--tool celia) renders either:
--   frames.bin  per frame 4 x 128x128 RGBA8 layers: the screen (the palette
--               INDEX is in red, index = round(r * 15 / 255)), the player
--               layer (player or player_spawn with its hair, alpha 0
--               elsewhere), the smoke layer, the objects layer (fruit,
--               fly_fruit with its wings, the lifeup "1000", balloons);
--   frames.jsonl per frame: the input the step consumed (-1 before the
--               player is active), its index in the file (keypress_frame),
--               Celia's frame count after it (practice_time: the clean
--               save's count),
--               the player, the deaths, and whether the room changed (`exit`).
-- One record per cctas step of the playback (driver.lua's "wait" phase),
-- taken right after the step, before the clean save's seek rewinds the
-- exit frame away.
--
-- Unlike UCT, Celia deep-copies the whole pico8 state (cart closures and
-- their upvalues included) on every step, so nothing here hooks the cart:
-- the layers are drawn AFTER the step, by calling the cart's draw_object
-- again on the step's final state, into separate canvases, with every
-- scalar field of the object (lifeup's flash) and its hair saved and
-- restored around it. The game's own frame and state are unchanged.
local C = {}

local LAYER_OF = {player = 1, player_spawn = 1, smoke = 2, fruit = 3, fly_fruit = 3, lifeup = 3, balloon = 3}

local function snapshot(o)
  local s = {}
  for k, v in pairs(o) do
    if type(v) ~= "table" and type(v) ~= "function" then s[k] = v end
  end
  if o.hair then
    s.__hair = {}
    for i, h in ipairs(o.hair) do s.__hair[i] = {h.x, h.y, h.size} end
  end
  return s
end

local function restore(o, s)
  for k, v in pairs(s) do
    if k ~= "__hair" then o[k] = v end
  end
  if s.__hair then
    for i, h in ipairs(o.hair) do h.x, h.y, h.size = s.__hair[i][1], s.__hair[i][2], s.__hair[i][3] end
  end
end

function C.install(drv)
  -- Celia's rnd is LOVE's global generator, seeded from the clock: fixed
  -- here (before love.load runs the cart's init), so two captures share
  -- their clouds and particles. Only cosmetic draws and smoke use it in
  -- this cart; the balloons' seeds come from the files.
  love.math.setRandomSeed(1)
  math.randomseed(1)
  C.drv = drv
  C.dir = os.getenv("CELIA_CAPTURE")
  C.bin = assert(io.open(C.dir .. "/frames.bin", "wb"))
  C.meta = assert(io.open(C.dir .. "/frames.jsonl", "w"))
  C.layers = {love.graphics.newCanvas(128, 128), love.graphics.newCanvas(128, 128), love.graphics.newCanvas(128, 128)}
  C.n = 0
end

-- Wrap the tool's step once the driver has the tool (drv.tas).
function C.wrap(tas)
  if C.wrapped then return end
  C.wrapped = true
  local step = tas.step
  tas.step = function(self)
    if C.drv.phase ~= "wait" then return step(self) end
    local kf = self:frame_count() + 1
    local input = self.keystates[kf] or 0
    local lvl = self:level_index()
    step(self)
    -- Before the player is active (the frame count stays 0) Celia feeds
    -- the first input to the cart but does not count it: shown as none, as UCT does.
    C.record(self, self:frame_count() > 0 and input or -1, kf, lvl ~= self:level_index())
  end
end

local function draw_layers()
  local cart = pico8.cart
  for _, c in ipairs(C.layers) do
    love.graphics.setCanvas(c)
    love.graphics.clear(0, 0, 0, 0)
  end
  love.graphics.push()
  restore_clip()
  restore_camera()
  for _, o in ipairs(cart.objects) do
    local name
    for k, v in pairs(LAYER_OF) do
      if cart[k] ~= nil and o.type == cart[k] then name = k end
    end
    if name then
      local s = snapshot(o)
      love.graphics.setCanvas(C.layers[LAYER_OF[name]])
      cart.draw_object(o)
      restore(o, s)
    end
  end
  love.graphics.pop()
end

function C.record(tas, input, kf, exited)
  local cart = pico8.cart
  -- A freeze frame draws nothing (`_draw` returns early): keep the layers too.
  if (cart.freeze or 0) <= 0 or C.n == 0 then
    draw_layers()
  end
  love.graphics.setCanvas()
  for _, c in ipairs({pico8.screen, C.layers[1], C.layers[2], C.layers[3]}) do
    C.bin:write(c:newImageData():getString())
  end
  love.graphics.setCanvas(pico8.screen)
  local px, py, dj = "null", "null", "null"
  for _, o in ipairs(cart.objects) do
    if o.type == cart.player then px, py, dj = o.x, o.y, o.djump end
  end
  C.meta:write(string.format(
    '{"n":%d,"input":%d,"keypress_frame":%d,"practice_time":%d,"exit":%s,"freeze":%d,"room":[%d,%d],"player":[%s,%s],"djump":%s,"max_djump":%d,"deaths":%d}\n',
    C.n, input, kf, tas:frame_count(), tostring(exited), cart.freeze or 0, cart.room.x, cart.room.y,
    tostring(px), tostring(py), tostring(dj), cart.max_djump, cart.deaths or 0))
  C.n = C.n + 1
  C.bin:flush(); C.meta:flush()
end

return C
