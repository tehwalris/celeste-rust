pico-8 cartridge // http://www.pico-8.com
version 42
__lua__
-- the provenance of cart/pico8_sin_table.bin (crates/celeste-core
-- pico8_num.rs): pico-8's `sin` at every input it can distinguish, as exact
-- 16.16 hex. run it on a real pico-8 and fold the output:
--
--   pico8 -x cart/pico8_sin_table.p8 2>/dev/null | grep '^0x' > /tmp/sintable.txt
--
-- two facts, both verified over all 65536 inputs rather than assumed:
-- * `sin` is constant over blocks of two adjacent inputs, so the low bit of
--   the 16-bit fraction is dead and 32768 entries suffice;
-- * `sin(x + 0.5) == -sin(x)` exactly, so only the first half-turn is stored.
-- together: 16384 little-endian i32, indexed by `(frac & 0x7fff) >> 1`,
-- negated when `frac >= 32768`. the reconstruction was checked against all
-- 65536 console values with zero mismatches. `sin(0.5 - x) == sin(x)` does
-- not hold, though a real sine says it should: the console's table is
-- asymmetric under rounding, which is why it is a table and not a formula.
--
-- 65536 is not representable in 16.16, so `i/65536` is garbage; 0x0000.0001
-- is exactly one ulp, so i*ulp walks the domain exactly.
local ulp=0x0000.0001
for i=0,32767 do printh(tostr(sin(i*ulp),true)) end
for i=0,32767 do printh(tostr(sin(i*ulp+0.5),true)) end
printh("DONE")
