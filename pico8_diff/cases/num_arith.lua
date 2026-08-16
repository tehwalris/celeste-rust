-- Add, subtract and multiply, including what happens at the ends of the
-- 16.16 range.
--
-- PICO-8 does NOT saturate on +, - or *: it wraps two's complement, so
-- 32767 + 1 is -32768. Our `Add`/`Sub` use `wrapping_add`/`wrapping_sub` and
-- `const_mul` keeps the low 32 bits of the 64-bit product, which is the same
-- model - this case is what makes that agreement measured.
--
-- The multiplication lines with a negative operand and a non-exact product
-- are the interesting ones: our `const_mul` finishes with `>> 16` on an i64,
-- which rounds toward NEGATIVE INFINITY, whereas a C implementation that
-- divides would round toward zero. The two differ by one ulp for exactly
-- these inputs.

__hex(1 + 1)
__hex(0.5 + 0.5)
__hex(0.1 + 0.2)
__hex(1 - 2)
__hex(0.1 - 0.2)
__hex(-1 - 1)

-- Wrap at the top and bottom of the range.
__hex(32767 + 1)
__hex(32767 + 32767)
__hex(-32767 - 1)
__hex(-32767 - 2)
__hex(-32767 - 32767)
__hex(32767 + 0.99999)

__hex(2 * 3)
__hex(-2 * 3)
__hex(-2 * -3)
__hex(0.5 * 0.5)
__hex(0.25 * 0.5)
-- Non-exact products: 0.1 * 0.1 needs bits below the last one.
__hex(0.1 * 0.1)
__hex(-0.1 * 0.1)
__hex(0.1 * -0.1)
__hex(-0.1 * -0.1)
__hex(0.3 * 0.7)
__hex(-0.3 * 0.7)
__hex(0.7 * -0.3)

-- Multiplication overflow wraps too. `mn` is the bottom of the range built
-- by arithmetic: the literal `-32768` does not survive our parser (see
-- num_literals.lua) and this case is not about parsing.
mn = -32767 - 1
__hex(32767 * 2)
__hex(mn * -1)
__hex(mn * 2)
__hex(256 * 256)
__hex(-256 * 256)
