-- The forty arguments the game's fruit bob actually evaluates `sin` at.
--
-- The fruit's vertical offset is driven by `sin(off / 40)` with an integer
-- `off`, and `Pico8Num::pico8_sin`'s doc comment says outright that full
-- bit-exactness against a real console has not been verified and that "a
-- dumped table from real PICO-8 can pin all of them". This is that dump,
-- expressed as a case so it stays pinned.
--
-- One residue being off by an ulp is enough to move a fruit's collision box
-- by a subpixel on some frame, so a proof that depends on fruit collection
-- timing depends on every line of this case.

for off = 0, 39 do
  __hex(sin(off / 40))
end

-- Periodicity: the widening in `apply_conservative_widenings` assumes
-- sin(off/40) is invariant under off -> off + 40, which our implementation
-- guarantees by construction (it reduces mod one turn in fixed point). The
-- console has to agree.
for off = 40, 51 do
  __hex(sin(off / 40))
end
