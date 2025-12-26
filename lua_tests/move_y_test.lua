-- Test Y movement specifically
-- This is to debug why y position isn't changing despite non-zero spd.y

function make_player(x, y, spd_x, spd_y)
    local obj = {}
    obj.x = x
    obj.y = y
    obj.spd = { x = spd_x, y = spd_y }
    obj.rem = { x = 0, y = 0 }
    obj.solids = false

    obj.move = function(ox, oy)
        __print("move called with ox=" .. ox .. " oy=" .. oy)
        local amount
        -- [x]
        obj.rem.x = obj.rem.x + ox + 0.5
        amount = flr(obj.rem.x)
        obj.rem.x = obj.rem.x - 0.5 - amount
        obj.move_x(amount, 0)

        -- [y]
        obj.rem.y = obj.rem.y + oy + 0.5
        amount = flr(obj.rem.y)
        __print("y amount=" .. amount)
        obj.rem.y = obj.rem.y - 0.5 - amount
        obj.move_y(amount)
    end

    obj.move_x = function(amount, start)
        obj.x = obj.x + amount
    end

    obj.move_y = function(amount)
        __print("move_y called with amount=" .. amount)
        obj.y = obj.y + amount
        __print("y is now " .. obj.y)
    end

    return obj
end

-- Test: Move up (negative y velocity, like jumping)
__print("Test: Move up with spd.y = -2")
local obj = make_player(8, 112, 0, -2)
__print("Before: y=" .. obj.y)

-- Simulate the _update loop condition
if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
    __print("Calling move")
    obj.move(obj.spd.x, obj.spd.y)
else
    __print("NOT calling move")
end

__print("After: y=" .. obj.y)

-- Expected: y should be 110 (112 + (-2) = 110)
