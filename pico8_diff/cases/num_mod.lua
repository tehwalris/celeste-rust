-- `%` with a POSITIVE INTEGER divisor - the only shape our `checked_rem`
-- claims to model. The rest of the sign/zero space is num_mod_signs.lua.
--
-- The negative-dividend lines are the ones that matter for the game: the
-- interpreter's `rem` ladder relies on -2 % 8 being 6, not -2.

__hex(7 % 3)
__hex(-7 % 3)
__hex(0 % 3)
__hex(3 % 3)
__hex(-3 % 3)
__hex(6 % 3)
__hex(-6 % 3)
__hex(1 % 3)
__hex(-1 % 3)

__hex(-2 % 8)
__hex(-16 % 8)
__hex(10 % 8)

-- Fractional dividend, integer divisor.
__hex(7.5 % 3)
__hex(-7.5 % 3)
__hex(2.5 % 8)
__hex(-2.5 % 8)
__hex(0.1 % 1)
__hex(-0.1 % 1)

-- Range extremes. `mn` rather than the literal `-32768`: our parser
-- mishandles that literal (see num_literals.lua) and this case is about `%`.
mn = -32767 - 1
__hex(32767 % 8)
__hex(mn % 8)
__hex(32767 % 1)
__hex(mn % 1)
__hex(mn % 32767)
