-- `foreach` when the callback changes the table.
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
--                     lua/builtin_level_3.lua implements.
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
-- Each scenario prints its name, then every element the callback is handed,
-- then the table's length (no `..`: the tracer has none). `t` is a global.

visit = printh

-- 1. The callback deletes the element it was just handed. Under an index
--    walk everything after it shifts down one and "c" is never visited.
printh("delete_current")
t = {}
add(t, "a") add(t, "b") add(t, "c") add(t, "d")
foreach(t, function(x)
  visit(x)
  if x == "b" then del(t, x) end
end)
printh(#t)

-- 2. The callback deletes an element that has not been reached yet. A
--    snapshot still visits it; PICO-8 does not.
printh("delete_later")
t = {}
add(t, "a") add(t, "b") add(t, "c")
foreach(t, function(x)
  visit(x)
  if x == "a" then del(t, "b") end
end)
printh(#t)

-- 3. The callback appends. A snapshot never sees the new element; PICO-8
--    visits it.
printh("append")
t = {}
add(t, "a")
foreach(t, function(x)
  visit(x)
  if x == "a" then add(t, "z") end
end)
printh(#t)

-- 4. The callback deletes an element BEFORE the current one, so the current
--    index now points one past where it should. An index walk drops the
--    tail element.
printh("delete_earlier")
t = {}
add(t, "a") add(t, "b") add(t, "c")
foreach(t, function(x)
  visit(x)
  if x == "b" then del(t, "a") end
end)
printh(#t)

-- 5. Sole element deletes itself: the table goes empty while the iterator
--    is mid-flight.
printh("single_self_delete")
t = {}
add(t, "a")
foreach(t, function(x)
  visit(x)
  del(t, x)
end)
printh(#t)

-- 6. Empty table: the callback must not run at all, and in particular must
--    not be handed nil.
printh("empty")
t = {}
foreach(t, function(x)
  visit("SHOULD_NOT_RUN")
end)
printh(#t)
