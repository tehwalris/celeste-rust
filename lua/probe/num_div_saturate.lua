-- Division whose result does not fit in 16.16, including division by zero.
-- PICO-8 SATURATES, with one rule for both: the quotient is clamped to
-- [0x8000.0001, 0x7fff.ffff]. No NaN, no infinity, and the negative clamp is
-- 0x8000.0001, not 0x8000.0000.

mn = -32767 - 1

__hex(32767 / 0.5)
__hex(-32767 / 0.5)
__hex(1 / 0.00002)
__hex(mn / 0.5)
__hex(32767 / 0.00002)

__hex(1 / 0)
__hex(-1 / 0)
__hex(0 / 0)
__hex(0.5 / 0)
__hex(-0.5 / 0)
__hex(32767 / 0)
__hex(mn / 0)
