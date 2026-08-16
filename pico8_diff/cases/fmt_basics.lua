-- Pins the two output channels themselves, so that a failure in any other
-- case means a semantic difference and not a formatting one.
--
-- `__print` is only ever used for strings, booleans and INTEGER-valued
-- numbers; our `format_scalar_number` and PICO-8's `tostr` are known to
-- disagree on fractions (we print `whole.raw_fraction`, e.g. 0.5 as
-- "0.32768"), which is why every fractional value in this suite goes through
-- `__hex` instead. This case is what makes that "integers only" claim
-- checked rather than assumed.

__print("string")
__print("")
__print("with spaces and 0x2e.")
__print(true)
__print(false)

__print(0)
__print(1)
__print(-1)
__print(9)
__print(10)
__print(255)
__print(32767)
__print(-32767)
-- The bottom of the whole-part range, reached by arithmetic rather than by
-- the literal `-32768`: our parser mishandles that literal (see
-- num_literals.lua), and this case is about FORMATTING, so it must not fail
-- for a parsing reason.
__print(-32767 - 1)

__hex(0)
__hex(1)
__hex(-1)
__hex(32767)
__hex(-32767 - 1)
