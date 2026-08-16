-- `flr` rounds toward NEGATIVE INFINITY, not toward zero: flr(-0.5) is -1.
--
-- Ours is an arithmetic right shift by 16, which is floor by construction.
-- The negative lines are what tells floor apart from truncation, and the
-- `-0.00002` line is the one-ulp case where the two differ on a value the
-- game's subpixel arithmetic actually produces.

__hex(flr(0))
__hex(flr(1))
__hex(flr(0.5))
__hex(flr(0.99999))
__hex(flr(0.00002))

__hex(flr(-1))
__hex(flr(-0.5))
__hex(flr(-0.00002))
__hex(flr(-0.99999))
__hex(flr(-1.5))
__hex(flr(-2))

-- The bottom of the range, built by arithmetic: the literal `-32768` does
-- not survive our parser (see num_literals.lua) and this case is about flr.
mn = -32767 - 1
__hex(flr(32767))
__hex(flr(mn))
__hex(flr(255.5))
__hex(flr(-255.5))

-- flr composed with the arithmetic that feeds it in the game.
__hex(flr(1 / 3))
__hex(flr(-1 / 3))
__hex(flr(7 / 2))
__hex(flr(-7 / 2))
