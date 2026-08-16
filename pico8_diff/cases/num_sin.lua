-- PICO-8's `sin` takes TURNS, not radians, and is INVERTED relative to a
-- normal sine: sin(0.25) = -1.
--
-- Ours (`Pico8Num::pico8_sin`) reduces the argument mod one turn in fixed
-- point and then runs `-sinf(2*pi*turns)` in f32, truncating the result
-- toward zero. That is a MODEL of the console, not a reproduction of it: the
-- console's own sine is a table lookup, so agreement anywhere except the
-- quarter turns has to be measured. This case measures it.

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
