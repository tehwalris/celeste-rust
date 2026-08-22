-- Table semantics, as a DIFFERENTIAL corpus against real PICO-8.
--
-- Every case prints a label and then one value per `printh`. No string
-- concatenation and no `tostr`: the tracer implements neither, and the
-- point is that this exact source runs in both places.
--
-- What it is for: `#` on a table with holes is often described as
-- "undefined", which invites guessing. It is not undefined - it is a
-- deterministic consequence of Lua's array-part/hash-part split, and it
-- depends on how the table was BUILT rather than on what it contains.
-- `t={} t[3]="c"` and `{"a",nil,"c"}` hold the same values and have
-- different lengths. So the model has to be checked, not reasoned about.
--
-- Regenerate the golden file with ./regen-pico8-golden.sh (needs PICO-8).

function dump(t)
  printh(#t)
  printh(t[1]) printh(t[2]) printh(t[3]) printh(t[4]) printh(t[5])
end

-- The project's shims, as `lua/builtin_level_4.lua` has them, so that the
-- generic index/length paths they are built on get exercised too.
-- `__array_table_drop_last` is a native there; here it is what it means.
function s_count(v) return #v end
function s_add(list, v) list[#list+1] = v end
function s_del(list, target)
  if #list == 0 then return nil end
  local found = false
  local found_value = nil
  for i=1,32767 do
    if i > #list then break end
    if not found then
      local v = list[i]
      if v == target then found = true found_value = v end
    end
    if found and (i + 1) <= #list then list[i] = list[i + 1] end
  end
  if found then list[#list] = nil end
  return found_value
end
function s_foreach(list, f) for i=1,#list do f(list[i]) end end

-- 1. An index written past the end of the array part goes to the hash
--    part, and the array part only absorbs it once the gap is filled.
printh("sparse-build")
local t={}
dump(t)
t[3]="c" dump(t)
t[1]="a" dump(t)
t[2]="b" dump(t)

-- 2. A CONSTRUCTOR sizes the array part up front, so the same contents
--    as case 1 step 2 have a different length.
printh("constructor-hole")
local k={"a",nil,"c"}
dump(k)

printh("constructor-dense")
local k2={"a","b","c"}
dump(k2)

-- 3. Punching a hole in a dense table, in the middle and at the end.
printh("nil-middle")
local r={"a","b","c"}
r[2]=nil
dump(r)

printh("nil-last")
local r2={"a","b","c"}
r2[3]=nil
dump(r2)

printh("nil-last-then-length")
local r3={"a","b","c"}
r3[3]=nil
r3[#r3+1]="z"
dump(r3)

-- 4. Appending.
printh("append")
local ap={}
s_add(ap,"a") dump(ap)
s_add(ap,"b") dump(ap)

printh("append-to-hole")
local ah={}
ah[3]="c"
s_add(ah,"x")
dump(ah)

-- 5. del, which is the shim the cart actually runs.
printh("del-middle")
local d1={"a","b","c"} s_del(d1,"b") dump(d1)
printh("del-first")
local d2={"a","b","c"} s_del(d2,"a") dump(d2)
printh("del-last")
local d3={"a","b","c"} s_del(d3,"c") dump(d3)
printh("del-absent")
local d4={"a","b"} s_del(d4,"zz") dump(d4)
printh("del-only")
local d5={"a"} s_del(d5,"a") dump(d5)
printh("del-empty")
local d6={} s_del(d6,"a") dump(d6)

-- 6. Iteration over a hole-punched table: how many times does the body
--    run, and with what?
printh("foreach-hole")
local fh={}
fh[1]="a"
fh[3]="c"
s_foreach(fh, function(e) printh(e) end)
printh(s_count(fh))

printh("for-over-length")
local fl={"a","b","c"}
fl[2]=nil
for i=1,#fl do printh(fl[i]) end

-- 7. Reads that are not in the array part at all.
printh("odd-indices")
local o={"a"}
printh(o[0])
printh(o[2])
printh(o[100])

-- 8. Nesting, since the cart's objects are tables of tables.
printh("nested")
local n={}
n[1]={}
n[1][2]="deep"
printh(#n)
printh(n[1][1])
printh(n[1][2])
printh(#n[1])
