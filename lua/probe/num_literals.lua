-- How a decimal literal in the source becomes 16.16 bits: PICO-8 TRUNCATES
-- rather than rounds (0.1 -> 0x0000.1999, not 0x0000.199a), measured here
-- rather than assumed.

__hex(0)
__hex(1)
__hex(2)
__hex(0.5)
__hex(0.25)
__hex(0.125)
__hex(1.5)
__hex(0.1)
__hex(0.2)
__hex(0.3)
__hex(0.7)
__hex(0.9)
__hex(0.99999)
-- One ulp and just under one ulp: the truncation boundary.
__hex(0.00002)
__hex(0.00001)
__hex(0.000005)
-- Whole-part extremes. 32768 is out of range as written (PICO-8 wraps it,
-- the interpreter RAISES): num_literals_overflow.lua.
__hex(255)
__hex(256)
__hex(32767)
__hex(-1)
__hex(-0.5)
__hex(-0.1)
__hex(-32767)
