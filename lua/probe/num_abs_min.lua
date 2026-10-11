-- `abs` at the one input whose answer is not representable: PICO-8
-- SATURATES, abs(-32768) = 0x7fff.ffff (a wrap would make a distance or a
-- speed magnitude NEGATIVE). The value is built by arithmetic, not written as
-- the literal `-32768` (num_literals.lua): this corpus is about `abs`.

mn = -32767 - 1

__hex(mn)
__hex(abs(mn))
__hex(abs(0 - 32767 - 1))
__hex(abs(mn) - 1)
