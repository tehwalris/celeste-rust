-- Test foreach with objects that have speed, similar to _update loop

function foreach(tbl, func)
  for i=1,32767 do
    if #tbl < i then
      break
    end
    func(tbl[i])
  end
end

-- Create objects array with one object
objects = {}

function make_player(x, y, spd_x, spd_y)
    local obj = {}
    obj.x = x
    obj.y = y
    obj.spd = { x = spd_x, y = spd_y }
    obj.rem = { x = 0, y = 0 }
    obj.solids = false

    obj.move = function(ox, oy)
        local amount
        obj.rem.x = obj.rem.x + ox + 0.5
        amount = flr(obj.rem.x)
        obj.rem.x = obj.rem.x - 0.5 - amount
        obj.move_x(amount, 0)

        obj.rem.y = obj.rem.y + oy + 0.5
        amount = flr(obj.rem.y)
        obj.rem.y = obj.rem.y - 0.5 - amount
        obj.move_y(amount)
    end

    obj.move_x = function(amount, start)
        obj.x = obj.x + amount
    end

    obj.move_y = function(amount)
        obj.y = obj.y + amount
    end

    return obj
end

-- Test 1: Single object with non-zero speed
__print("Test 1: foreach with object spd=(2,0)")
objects = {}
add(objects, make_player(8, 112, 2, 0))
__print(objects[1].x)  -- 8

foreach(objects, function(obj)
    if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
        obj.move(obj.spd.x, obj.spd.y)
    end
end)
__print(objects[1].x)  -- Should be 10

-- Test 2: Object with zero speed (should NOT move)
__print("Test 2: foreach with object spd=(0,0)")
objects = {}
add(objects, make_player(8, 112, 0, 0))
__print(objects[1].x)  -- 8

foreach(objects, function(obj)
    if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
        obj.move(obj.spd.x, obj.spd.y)
    end
end)
__print(objects[1].x)  -- Should still be 8

-- Test 3: Test with negative speed
__print("Test 3: foreach with object spd=(-2,0)")
objects = {}
add(objects, make_player(8, 112, -2, 0))
__print(objects[1].x)  -- 8

foreach(objects, function(obj)
    if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
        obj.move(obj.spd.x, obj.spd.y)
    end
end)
__print(objects[1].x)  -- Should be 6

-- Test 4: Multiple objects
__print("Test 4: multiple objects")
objects = {}
add(objects, make_player(8, 112, 2, 0))
add(objects, make_player(10, 100, -1, 0))

foreach(objects, function(obj)
    if obj.spd.x ~= 0 or obj.spd.y ~= 0 then
        obj.move(obj.spd.x, obj.spd.y)
    end
end)
__print(objects[1].x)  -- Should be 10
__print(objects[2].x)  -- Should be 9
