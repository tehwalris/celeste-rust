function add(t, v)
  t[#t + 1] = v
end

function foreach(tbl, func)
  for i=1,32767 do
    if #tbl < i then
      break
    end
    func(tbl[i])
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