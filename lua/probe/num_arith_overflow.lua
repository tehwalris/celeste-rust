-- num_arith.lua's cases past the 16.16 range: PICO-8 WRAPS +, - and * two's
-- complement (32767 + 1 is -32768); the interpreter RAISES on each
-- (src/trace/probe.rs, OVERFLOW).

-- Wrap at the top and bottom of the range.
__hex(32767 + 1)
__hex(32767 + 32767)
__hex(-32767 - 2)
__hex(-32767 - 32767)

-- Multiplication overflow wraps too. `mn` is the bottom of the range built
-- by arithmetic (the literal: num_literals.lua).
mn = -32767 - 1
__hex(32767 * 2)
__hex(mn * -1)
__hex(mn * 2)
__hex(256 * 256)
__hex(-256 * 256)
