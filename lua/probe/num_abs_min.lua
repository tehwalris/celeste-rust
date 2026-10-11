-- `abs` at the one input whose answer is not representable: PICO-8
-- SATURATES, abs(-32768) = 0x7fff.ffff (a wrap would make a distance or a
-- speed magnitude NEGATIVE). The value is built by arithmetic, not written as
-- the literal `-32768` (num_literals.lua): this corpus is about `abs`. The
-- interpreter RAISES on an overflow, so the `abs` cases themselves are
-- num_abs_min_overflow.lua; here, the input they take.

mn = -32767 - 1

__hex(mn)
