-- `%` with a POSITIVE INTEGER divisor (every other divisor:
-- num_mod_signs.lua). The negative dividends matter to the game: -2 % 8 is
-- 6, not -2.

__hex(7 % 3)
__hex(-7 % 3)
__hex(0 % 3)
__hex(3 % 3)
__hex(-3 % 3)
__hex(6 % 3)
__hex(-6 % 3)
__hex(1 % 3)
__hex(-1 % 3)

__hex(-2 % 8)
__hex(-16 % 8)
__hex(10 % 8)

-- Fractional dividend, integer divisor.
__hex(7.5 % 3)
__hex(-7.5 % 3)
__hex(2.5 % 8)
__hex(-2.5 % 8)
__hex(0.1 % 1)
__hex(-0.1 % 1)

-- Range extremes. `mn`: the bottom of the range built by arithmetic (the
-- literal: num_literals.lua).
mn = -32767 - 1
__hex(32767 % 8)
__hex(mn % 8)
__hex(32767 % 1)
__hex(mn % 1)
__hex(mn % 32767)
