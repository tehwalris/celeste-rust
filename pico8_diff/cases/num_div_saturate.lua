-- Division whose result does not fit in 16.16, including division by zero.
--
-- PICO-8 SATURATES here, and it is the same rule for both: the quotient is
-- clamped to [0x8000.0001, 0x7fff.ffff]. There is no NaN and no infinity, and
-- note that the negative clamp is 0x8000.0001, not 0x8000.0000.
--
-- Ours does neither. `Div` computes `((self << 16) as i64).wrapping_div(rhs)`
-- and narrows with `as i32`, so an out-of-range quotient WRAPS (32767 / 0.5
-- comes out as -2), and a zero divisor makes `wrapping_div` PANIC and takes
-- the process down. Kept separate from num_div.lua so that the ordinary
-- division lines, which do agree, are not hidden behind this.
--
-- The overflow lines come first: our side dies at the first `/ 0`, so
-- anything after it produces no output at all.

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
