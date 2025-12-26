-- Test: Apply specific input sequence and observe positions
-- Jump on frame 1, then observe trajectory

-- Minimal game state
local player = {
  x = 8,
  y = 112,
  spd_x = 0,
  spd_y = 0,
  grace = 6,
  p_jump = false,
}

-- Update logic (simplified from Celeste)
function update_player(btn_jump)
  local on_ground = (player.y >= 112)
  
  -- Grace (coyote time)
  if on_ground then
    player.grace = 6
  elseif player.grace > 0 then
    player.grace = player.grace - 1
  end
  
  -- Jump
  local jump = btn_jump and not player.p_jump
  player.p_jump = btn_jump
  
  if jump and player.grace > 0 then
    player.grace = 0
    player.spd_y = -2  -- Celeste uses -2 for jump
  end
  
  -- Gravity (approximately 0.21 per frame)
  if not on_ground then
    player.spd_y = player.spd_y + 0.21
  end
  
  -- Apply velocity
  player.y = player.y + player.spd_y
  
  -- Floor collision
  if player.y > 112 then
    player.y = 112
    player.spd_y = 0
  end
end

-- Input sequence: jump on frame 1
local inputs = {true, false, false, false, false, false, false, false, false, false}

__print("Testing jump trajectory:")
__print("Frame 0: y=" .. player.y .. " spd_y=" .. player.spd_y)

for frame = 1, 10 do
  local btn_jump = inputs[frame] or false
  update_player(btn_jump)
  __print("Frame " .. frame .. ": y=" .. player.y .. " spd_y=" .. player.spd_y .. " input=" .. (btn_jump and "JUMP" or "-"))
end
