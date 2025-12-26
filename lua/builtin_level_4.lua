function __reset_button_states()
  __button_states = {}
  for i = 1,6 do
    __button_states[i] = __new_unknown_boolean()
  end
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