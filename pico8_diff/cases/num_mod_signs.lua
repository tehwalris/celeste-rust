-- `%` over the whole sign space, plus a zero divisor and fractional
-- divisors.
--
-- Our `checked_rem` refuses everything here (it returns None for a
-- non-positive or fractional divisor, and the interpreter turns that into an
-- error), so today this case fails on the first line. It is written anyway
-- because the point is to LEARN the rule from the console rather than
-- theorise it: the doc comment on `checked_rem` currently says PICO-8's `%`
-- is floored and "the result takes the divisor's sign", which would make
-- 7 % -3 equal -2.
--
-- Every sign combination and both fractional-divisor signs are here because
-- two data points do not determine the rule.

-- Positive divisor, both dividend signs (repeated from num_mod.lua so this
-- file is readable on its own as a table of the rule).
__hex(7 % 3)
__hex(-7 % 3)

-- Negative divisor.
__hex(7 % -3)
__hex(-7 % -3)
__hex(0 % -3)
__hex(3 % -3)
__hex(-3 % -3)
__hex(1 % -3)
__hex(-1 % -3)

-- Fractional divisor, both signs.
__hex(7 % 2.5)
__hex(-7 % 2.5)
__hex(7 % -2.5)
__hex(-7 % -2.5)
__hex(0.7 % 0.3)
__hex(-0.7 % 0.3)
__hex(0.7 % -0.3)

-- Zero divisor.
__hex(7 % 0)
__hex(-7 % 0)
__hex(0 % 0)
__hex(0.5 % 0)
