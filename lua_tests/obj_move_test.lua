-- Minimal test for obj.move functionality
-- This tests the core position update mechanism used in Celeste

-- Create a minimal object similar to how init_object works
function make_test_object(x, y)
    local obj = {}
    obj.x = x
    obj.y = y
    obj.spd = { x = 0, y = 0 }
    obj.rem = { x = 0, y = 0 }
    obj.solids = false  -- No collision for this test

    -- obj.move function (simplified from celeste-minimal.lua)
    obj.move = function(ox, oy)
        local amount
        -- [x] get move amount
        obj.rem.x = obj.rem.x + ox + 0.5
        obj.rem.x = obj.rem.x  -- Note: __split_by_flr removed for simplicity
        amount = flr(obj.rem.x)
        obj.rem.x = obj.rem.x - 0.5 - amount
        obj.move_x(amount, 0)

        -- [y] get move amount
        obj.rem.y = obj.rem.y + oy + 0.5
        obj.rem.y = obj.rem.y
        amount = flr(obj.rem.y)
        obj.rem.y = obj.rem.y - 0.5 - amount
        obj.move_y(amount)
    end

    obj.move_x = function(amount, start)
        -- No solids check for this test
        obj.x = obj.x + amount
    end

    obj.move_y = function(amount)
        -- No solids check for this test
        obj.y = obj.y + amount
    end

    return obj
end

-- Test 1: Moving with integer velocity
__print("Test 1")
local obj1 = make_test_object(8, 112)
__print(obj1.x)  -- Should print 8
obj1.move(2, 0)
__print(obj1.x)  -- Should print 10

-- Test 2: Moving with negative velocity
__print("Test 2")
local obj2 = make_test_object(8, 112)
__print(obj2.x)  -- Should print 8
obj2.move(-2, 0)
__print(obj2.x)  -- Should print 6

-- Test 3: Moving with fractional velocity (like 0.6 acceleration)
__print("Test 3")
local obj3 = make_test_object(8, 112)
__print(obj3.x)  -- Should print 8
obj3.move(0.6, 0)
__print(obj3.x)  -- Should print 9 (0.5 + 0.6 = 1.1, floor = 1, so 8 + 1 = 9)

-- Test 4: Test the condition check used in _update
__print("Test 4")
local spd_x = 2
local spd_y = 0
if spd_x ~= 0 or spd_y ~= 0 then
    __print("move called")
else
    __print("move NOT called")
end
