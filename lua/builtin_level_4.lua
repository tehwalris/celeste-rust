-- The button table is allocated ONCE, here at toplevel, so its identity and
-- shape are part of the initial heap and stable across frames. The reset
-- overwrites all six slots in place; it must never allocate. (A per-frame
-- `{}` rebuild made every btn read a multi-receiver site - the last 22
-- heap sites standing between the compiled per-shape programs and zero
-- heap. The toplevel values are dead: init ends with a reset.)
__button_states = {}
__button_states[1] = false
__button_states[2] = false
__button_states[3] = false
__button_states[4] = false
__button_states[5] = false
__button_states[6] = false

function __reset_button_states()
  __button_states[1] = __new_unknown_boolean()
  __button_states[2] = __new_unknown_boolean()
  __button_states[3] = __new_unknown_boolean()
  __button_states[4] = __new_unknown_boolean()
  __button_states[5] = __new_unknown_boolean()
  __button_states[6] = __new_unknown_boolean()
end

function btn(i)
  -- TODO What happens if this is called during _draw? Are the values guaranteed
  -- to be consistent with _update?

  __assert(i >= 0)
  __assert(i <= 5)
  i = i + 1
  -- This weird if statement concretizes the button
  -- state the first time it is read.
  if __button_states[i] then
    __button_states[i] = true
    return true
  else
    __button_states[i] = false
    return false
  end
end

function count(v)
  -- TODO crash when a second argument is called
  return #v
end

function del(list, target)
  if #list == 0 then
    return nil
  end

  local found = false
  local found_value = nil
  for i=1,32767 do
    if i > #list then
      break
    end
    if not found then
      local v = list[i]
      if v == target then
        found = true
        found_value = v
      end
    end
    if found and (i + 1) <= #list then
      list[i] = list[i + 1]
    end
  end

  if found then
    __array_table_drop_last(list)
  end

  return found_value
end

-- Noop functions

function print() end
function music() end
function sfx() end
function pal() end
function rectfill() end
function map() end
function spr() end
function circfill() end