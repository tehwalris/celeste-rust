-- Literals that need more than 24 significant bits.
--
-- Split out from num_literals.lua because our parse path is
-- `text.parse::<f32>()` -> `Pico8Num::from_f32`, and an f32 has a 24-bit
-- mantissa while 16.16 needs up to 31 significant bits. Any literal with a
-- large whole part AND a fine fraction therefore cannot survive the trip.
-- The game's own constants are all small, which is why this has never
-- mattered - but a rewrite or a hand-written test that types one of these
-- would get a silently wrong number.

__hex(100.5)
__hex(255.5)
__hex(256.00391)
__hex(1000.5)
__hex(1000.00002)
__hex(32767.5)
__hex(32767.99998)
__hex(-1000.00002)
__hex(-32767.99998)
