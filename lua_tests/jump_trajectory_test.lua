-- Simplified test for jump trajectory using only integers
-- This simulates basic jump physics: spd.y = -2 per frame, then gravity

-- Create a simple player object
function create_player(x, y)
  local obj = {}
  obj.x = x
  obj.y = y
  obj.spd_y = 0  -- Using separate field to avoid nested table
  obj.grace = 6
  obj.p_jump = false
  return obj
end

-- Simplified physics update
function update_player(obj, btn_jump)
  local on_ground = (obj.y >= 112)

  -- Grace (coyote time)
  if on_ground then
    obj.grace = 6
  elseif obj.grace > 0 then
    obj.grace = obj.grace - 1
  end

  -- Jump
  local jump = btn_jump and not obj.p_jump
  obj.p_jump = btn_jump

  if jump then
    if obj.grace > 0 then
      obj.grace = 0
      obj.spd_y = -2
    end
  end

  -- Gravity (simplified: just add 0.21 per frame when not on ground)
  -- Using integer approximation: after ~9 frames of gravity, spd changes by ~2
  if not on_ground then
    -- Approximate: every 5 frames, add 1 to spd_y
    -- For simplicity, just do nothing here - the jump test is about initial velocity
  end
end

-- Move based on speed (simplified: just integer movement)
function move_player(obj)
  if obj.spd_y ~= 0 then
    obj.y = obj.y + obj.spd_y
  end
end

-- Run simulation for N frames with specified jump inputs
function simulate(frames, jump_on_first)
  local player = create_player(8, 112)
  __print("Frame 0: y=" .. player.y .. " spd_y=" .. player.spd_y)

  for frame = 1, frames do
    local btn_jump = (frame == 1 and jump_on_first)

    -- Move first (using previous frame's velocity)
    move_player(player)

    -- Then update (sets velocity for next frame)
    update_player(player, btn_jump)

    __print("Frame " .. frame .. ": y=" .. player.y .. " spd_y=" .. player.spd_y)
  end

  return player.y
end

-- Test: Jump on frame 1
-- Expected: y should decrease by 2 per frame (spd_y = -2)
-- Frame 0: y=112, spd=0
-- Frame 1: move (no change), update (spd=-2) -> y=112
-- Frame 2: move (y=110), update -> y=110
-- Frame 3: move (y=108), update -> y=108
-- Frame 4: move (y=106), update -> y=106
-- Frame 5: move (y=104), update -> y=104

__print("=== Test: Jump trajectory ===")
local final_y = simulate(5, true)
__print("Final y: " .. final_y)
