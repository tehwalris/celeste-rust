pico-8 cartridge // http://www.pico-8.com
version 42
__lua__
-- 65536 is NOT representable in 16.16, so `i/65536` is garbage. 0x0000.0001
-- IS exactly one ulp, i.e. 1/65536, so i*ulp walks the domain exactly.
local ulp=0x0000.0001
for i=0,32767 do printh(tostr(sin(i*ulp),true)) end
for i=0,32767 do printh(tostr(sin(i*ulp+0.5),true)) end
printh("DONE")
