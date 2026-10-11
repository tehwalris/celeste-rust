-- The forty arguments the game's fruit bob evaluates `sin` at: `sin(off /
-- 40)` with an integer `off`. One residue off by an ulp moves a fruit's
-- collision box by a subpixel on some frame.

for off = 0, 39 do
  __hex(sin(off / 40))
end

-- Periodicity: a widening of the bob's phase assumes sin(off/40) is
-- invariant under off -> off + 40. The console has to agree.
for off = 40, 51 do
  __hex(sin(off / 40))
end
