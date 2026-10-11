-- Division whose result fits in 16.16 (one that does not, and division by
-- zero: num_div_saturate.lua). The sign combinations tell a quotient
-- truncated TOWARD ZERO from a floored one: -1/3 is 0xffff.aaab under
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
