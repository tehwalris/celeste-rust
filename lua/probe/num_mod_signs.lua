-- `%` over the whole sign space, a zero divisor and fractional divisors:
-- PICO-8's `%` is NOT Lua's (whose result takes the divisor's sign, 7 % -3 =
-- -2); every sign combination is here because two data points do not
-- determine the rule.

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
