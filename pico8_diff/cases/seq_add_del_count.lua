-- `add` / `del` / `count` / `#t` on the sequence shapes the game actually
-- produces. These are Lua-level in our build (lua/builtin_level_4.lua) and
-- native on PICO-8, so every line here is a real comparison.
--
-- Nothing prints a raw value that could be nil: `tostr(nil)` is "[nil]" on
-- PICO-8 and "nil" for us, which would be a formatting diff rather than a
-- semantic one. Nil-ness is printed as a boolean instead.

t = {}
__print(#t)
__print(count(t))

add(t, "a")
add(t, "b")
add(t, "c")
__print(#t)
__print(count(t))
__print(t[1])
__print(t[3])
__print(t[4] == nil)

-- PICO-8's `add` returns the value it appended. Our Lua-level `add` is
-- `t[#t+1] = v` and returns nothing, so this line is the check for that.
__print(add(t, "d") == "d")
__print(#t)

-- Deleting from the middle shifts the tail down and shortens the table.
__print(del(t, "b") == "b")
__print(#t)
__print(t[1])
__print(t[2])
__print(t[3])
__print(t[4] == nil)

-- Deleting something absent must return nil and change nothing.
__print(del(t, "zz") == nil)
__print(#t)

-- First and last elements.
__print(del(t, "a") == "a")
__print(t[1])
__print(del(t, "d") == "d")
__print(#t)
__print(t[1])

__print(del(t, "c") == "c")
__print(#t)
__print(t[1] == nil)

-- Deleting from an empty table.
__print(del(t, "c") == nil)
__print(#t)

-- Duplicates: only the FIRST occurrence goes.
t = {}
add(t, "x") add(t, "y") add(t, "x")
__print(del(t, "x") == "x")
__print(#t)
__print(t[1])
__print(t[2])

-- Numbers as elements, so that element equality is not only tested on
-- strings.
t = {}
add(t, 1) add(t, 2) add(t, 3)
__print(del(t, 2) == 2)
__print(#t)
__print(t[1])
__print(t[2])
__print(del(t, 99) == nil)
__print(#t)
