-- `min` / `max`. Ours are `Ord` on the raw i32, which is the right ordering
-- for a two's-complement fixed point - the interesting question is the
-- boundary values and ties, and whether PICO-8 returns the first or second
-- argument on a tie (invisible for numbers, but it pins the shape).

__hex(min(1, 2))
__hex(min(2, 1))
__hex(max(1, 2))
__hex(max(2, 1))
__hex(min(-1, 1))
__hex(max(-1, 1))
__hex(min(-1, -2))
__hex(max(-1, -2))
__hex(min(0.5, 0.25))
__hex(max(0.5, 0.25))
__hex(min(3, 3))
__hex(max(3, 3))
-- `mn` rather than the literal `-32768`: our parser mishandles that literal
-- (see num_literals.lua) and this case is about the ordering.
mn = -32767 - 1
__hex(min(mn, 32767))
__hex(max(mn, 32767))
__hex(min(0, -0.00002))
__hex(max(0, -0.00002))
__hex(min(1 / 3, 0.3))
__hex(max(1 / 3, 0.3))
