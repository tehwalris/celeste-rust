-- Literals that need more than 24 significant bits (a large whole part AND
-- a fine fraction): a parse through an f32 (24-bit mantissa) cannot carry
-- them, 16.16 needs up to 31. The game's own constants are all small.

__hex(100.5)
__hex(255.5)
__hex(256.00391)
__hex(1000.5)
__hex(1000.00002)
__hex(32767.5)
__hex(32767.99998)
__hex(-1000.00002)
__hex(-32767.99998)
