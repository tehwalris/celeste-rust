-- `add` / `del` / `count` / `#t` on the sequence shapes the game produces:
-- Lua-level in the tracer (lua/builtin_level_3.lua, _4), native on PICO-8.
-- Nothing prints a value that could be nil (`printh(nil)` would be a
-- formatting question): nil-ness is printed as a boolean.

t = {}
printh(#t)
printh(count(t))

add(t, "a")
add(t, "b")
add(t, "c")
printh(#t)
printh(count(t))
printh(t[1])
printh(t[3])
printh(t[4] == nil)

-- PICO-8's `add` returns the value it appended.
printh(add(t, "d") == "d")
printh(#t)

-- Deleting from the middle shifts the tail down and shortens the table.
printh(del(t, "b") == "b")
printh(#t)
printh(t[1])
printh(t[2])
printh(t[3])
printh(t[4] == nil)

-- Deleting something absent must return nil and change nothing.
printh(del(t, "zz") == nil)
printh(#t)

-- First and last elements.
printh(del(t, "a") == "a")
printh(t[1])
printh(del(t, "d") == "d")
printh(#t)
printh(t[1])

printh(del(t, "c") == "c")
printh(#t)
printh(t[1] == nil)

-- Deleting from an empty table.
printh(del(t, "c") == nil)
printh(#t)

-- Duplicates: only the FIRST occurrence goes.
t = {}
add(t, "x") add(t, "y") add(t, "x")
printh(del(t, "x") == "x")
printh(#t)
printh(t[1])
printh(t[2])

-- Numbers as elements, so that element equality is not only tested on
-- strings.
t = {}
add(t, 1) add(t, 2) add(t, 3)
printh(del(t, 2) == 2)
printh(#t)
printh(t[1])
printh(t[2])
printh(del(t, 99) == nil)
printh(#t)
