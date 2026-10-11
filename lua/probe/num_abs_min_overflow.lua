-- num_abs_min.lua's `abs` cases: PICO-8 SATURATES abs(-32768) to
-- 0x7fff.ffff; the interpreter RAISES on each (src/trace/probe.rs,
-- OVERFLOW).

mn = -32767 - 1

__hex(abs(mn))
__hex(abs(0 - 32767 - 1))
__hex(abs(mn) - 1)
