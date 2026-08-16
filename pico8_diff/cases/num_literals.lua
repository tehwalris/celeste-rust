-- How a decimal literal in the source becomes 16.16 bits.
--
-- Ours goes through `Pico8Num::from_f32` (src/pico8_num.rs), reached from
-- the frontend's `text.parse()?` on the number token. PICO-8 does its own
-- decimal-to-fixed conversion. Both appear to TRUNCATE rather than round
-- (0.1 -> 0x0000.1999, not 0x0000.199a), but "appear to" is exactly the kind
-- of claim this harness exists to replace.

__hex(0)
__hex(1)
__hex(2)
__hex(0.5)
__hex(0.25)
__hex(0.125)
__hex(1.5)
__hex(0.1)
__hex(0.2)
__hex(0.3)
__hex(0.7)
__hex(0.9)
__hex(0.99999)
-- One ulp and just under one ulp: the truncation boundary.
__hex(0.00002)
__hex(0.00001)
__hex(0.000005)
-- Whole-part extremes. 32768 is already out of range as written, so this
-- pins that both sides fold the out-of-range literal the same way.
__hex(255)
__hex(256)
__hex(32767)
__hex(32768)
__hex(-1)
__hex(-0.5)
__hex(-0.1)
__hex(-32767)
__hex(-32768)
