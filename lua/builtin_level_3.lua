function add(t, v)
  t[#t + 1] = v
end

function foreach(tbl, func)
  -- PICO-8 defines `foreach(t, f)` as `for item in all(t) do f(item) end`,
  -- and `all` is deletion-safe by REMEMBERING THE ITEM IT LAST RETURNED,
  -- not by snapshotting: it only advances the index when the current slot
  -- still holds that item. So when `func` deletes the element it was just
  -- handed, everything after shifts down one and the same index now holds
  -- the successor - which is therefore visited rather than skipped, as a
  -- plain `for i=1,#t` walk would.
  --
  -- The difference from a snapshot is not academic, and a snapshot is what
  -- was tried first: it visits elements deleted before their turn (PICO-8
  -- does not) and misses elements appended during the walk (PICO-8 visits
  -- them). It killed the player at frame 93 of the room (2, 0) community
  -- TAS, which is what sent me to look at `all` properly.
  --
  -- The evidence that THIS version is right is that community TAS: under
  -- the old index walk it never left the room, and under this one it exits
  -- at frame 95, exactly as recorded. Rooms (0, 0) and (1, 0) still exit at
  -- 94 and 100. A TAS that runs on real PICO-8 is the closest thing to a
  -- differential test against PICO-8 that we have.
  --
  -- TODO still test the rest of the builtins, and the numerics, against a
  -- real PICO-8 rather than against lore - which is what produced the
  -- snapshot version.
  local i = 1
  local prev = nil
  for step=1,32767 do
    if tbl[i] == prev then
      i = i + 1
    end
    prev = tbl[i]
    if prev == nil then
      break
    end
    func(prev)
  end
end

-- HACK This is not a pico8 function, but it's convenient to put it here
function __assert(cond, msg)
  if not cond then
    __print("Assertion failed")
    if msg == nil then
      error()
    else
      __print(msg)
      error(msg)
    end
  end
end