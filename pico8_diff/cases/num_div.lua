-- Division whose result fits in 16.16. Division by zero, and division whose
-- result does NOT fit, are in num_div_saturate.lua - both are known
-- divergences and would otherwise mask everything below them.
--
-- Our `Div` widens the dividend to i64, shifts left 16 and uses Rust's `/`,
-- which truncates TOWARD ZERO. The sign combinations below are what
-- distinguishes that from a floored division: -1/3 is 0xffff.aaab under
-- truncation and 0xffff.aaaa under flooring.

__hex(100 / 4)
__hex(-100 / 4)
__hex(100 / -4)
__hex(-100 / -4)

__hex(1 / 2)
__hex(1 / 3)
__hex(-1 / 3)
__hex(1 / -3)
__hex(-1 / -3)
__hex(2 / 3)
__hex(-2 / 3)

__hex(100 / 7)
__hex(-100 / 7)
__hex(100 / -7)
__hex(-100 / -7)

__hex(1 / 32767)
__hex(-1 / 32767)
__hex(0.5 / 2)
__hex(3 / 0.5)
__hex(-3 / 0.5)
__hex(0.1 / 0.3)
__hex(-0.1 / 0.3)
