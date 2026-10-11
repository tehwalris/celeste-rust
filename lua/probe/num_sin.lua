-- PICO-8's `sin` takes TURNS, not radians, and is INVERTED relative to a
-- normal sine: sin(0.25) = -1. The console's sine is a table lookup
-- (cart/pico8_sin_table.bin, from cart/pico8_sin_table.p8): agreement
-- anywhere but the quarter turns has to be measured.

__hex(sin(0))
__hex(sin(0.25))
__hex(sin(0.5))
__hex(sin(0.75))
__hex(sin(1))

-- Negative arguments and arguments past one turn: the reduction step.
__hex(sin(-0.25))
__hex(sin(-0.5))
__hex(sin(-1))
__hex(sin(1.25))
__hex(sin(2.5))
__hex(sin(-2.75))

-- Arbitrary points, where a table lookup and a real sine part company.
__hex(sin(0.125))
__hex(sin(0.025))
__hex(sin(0.1))
__hex(sin(0.3))
__hex(sin(0.4))
__hex(sin(0.6))
__hex(sin(0.9))
__hex(sin(0.00002))
