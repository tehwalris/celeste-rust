-- `abs` at the one input whose answer is not representable.
--
-- Real PICO-8 SATURATES: abs(-32768) = 0x7fff.ffff. Ours is
-- `Pico8Num(self.0.abs())`, and `i32::MIN.abs()` overflows - in a release
-- build that wraps back to i32::MIN, so we return -32768, and in a debug
-- build it panics. Either way it is wrong in the direction that matters: a
-- distance or a speed magnitude coming out NEGATIVE.
--
-- The value is built by arithmetic, not written as the literal `-32768`,
-- because our parser does not produce -32768 for that literal either (see
-- num_literals.lua) and this case is about `abs`.

mn = -32767 - 1

__hex(mn)
__hex(abs(mn))
__hex(abs(0 - 32767 - 1))
__hex(abs(mn) - 1)
