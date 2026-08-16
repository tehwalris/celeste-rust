-- The case this whole harness was built for.
--
-- Three candidate models of `foreach` have been in this repo or considered
-- for it:
--
--   (I)  INDEX WALK   `for i=1,32767 do if i > #t then break end f(t[i]) end`
--                     - what we shipped for months.
--   (S)  SNAPSHOT     copy `t` first, then walk the copy - the "fix" that
--                     was applied from lore, and that killed the player at
--                     frame 93 of the room (2, 0) community TAS.
--   (A)  ALL          `for item in all(t) do f(item) end`, where `all` keeps
--                     an index plus the last item it returned and only
--                     advances when that slot still holds it. This is what
--                     lua/builtin_level_3.lua implements today.
--
-- The six scenarios below are chosen so that (I) and (S) each fail a
-- DIFFERENT pair, which is why no smaller set pins the semantics down:
--
--   scenario            (I)                  (S)                  (A)
--   delete current      "abd"   WRONG        "abcd"               "abcd"
--   delete later        "ac"                 "abc"   WRONG        "ac"
--   append              "az"                 "a"     WRONG        "az"
--   delete earlier      "ab"    WRONG        "abc"                "abc"
--   single self-delete  "a"                  "a"                  "a"
--   empty               ""                   ""                   ""
--
-- (I) fails "delete current" and "delete earlier"; (S) fails "delete later"
-- and "append". The last two scenarios discriminate nothing on their own -
-- they are here because they are the degenerate shapes where an
-- index-plus-last-item scheme is easiest to get wrong.
--
-- Tables are built with `add` rather than `{"a","b"}` because our frontend
-- only compiles table constructors with named fields. `t` is a global
-- because the closures below would otherwise need an upvalue.

visited = ""

function visit(x)
  visited = visited .. x
end

-- 1. The callback deletes the element it was just handed. Under an index
--    walk everything after it shifts down one and "c" is never visited.
t = {}
add(t, "a") add(t, "b") add(t, "c") add(t, "d")
visited = ""
foreach(t, function(x)
  visit(x)
  if x == "b" then del(t, x) end
end)
__print("delete_current")
__print(visited)
__print(#t)

-- 2. The callback deletes an element that has not been reached yet. A
--    snapshot still visits it; PICO-8 does not.
t = {}
add(t, "a") add(t, "b") add(t, "c")
visited = ""
foreach(t, function(x)
  visit(x)
  if x == "a" then del(t, "b") end
end)
__print("delete_later")
__print(visited)
__print(#t)

-- 3. The callback appends. A snapshot never sees the new element; PICO-8
--    visits it.
t = {}
add(t, "a")
visited = ""
foreach(t, function(x)
  visit(x)
  if x == "a" then add(t, "z") end
end)
__print("append")
__print(visited)
__print(#t)

-- 4. The callback deletes an element BEFORE the current one, so the current
--    index now points one past where it should. An index walk drops the
--    tail element.
t = {}
add(t, "a") add(t, "b") add(t, "c")
visited = ""
foreach(t, function(x)
  visit(x)
  if x == "b" then del(t, "a") end
end)
__print("delete_earlier")
__print(visited)
__print(#t)

-- 5. Sole element deletes itself: the table goes empty while the iterator
--    is mid-flight.
t = {}
add(t, "a")
visited = ""
foreach(t, function(x)
  visit(x)
  del(t, x)
end)
__print("single_self_delete")
__print(visited)
__print(#t)

-- 6. Empty table: the callback must not run at all, and in particular must
--    not be handed nil.
t = {}
visited = ""
foreach(t, function(x)
  visit("SHOULD_NOT_RUN")
end)
__print("empty")
__print(visited)
__print(#t)
