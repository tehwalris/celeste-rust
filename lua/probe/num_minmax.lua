-- `min` / `max`: ordering on two's-complement 16.16, at the range's ends and
-- on ties.

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
-- `mn`: the bottom of the range built by arithmetic (the literal:
-- num_literals.lua).
mn = -32767 - 1
__hex(min(mn, 32767))
__hex(max(mn, 32767))
__hex(min(0, -0.00002))
__hex(max(0, -0.00002))
__hex(min(1 / 3, 0.3))
__hex(max(1 / 3, 0.3))
