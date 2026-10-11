-- The print channels themselves, so that a failure in any other corpus means
-- a semantic difference and not a formatting one: `printh` of strings,
-- booleans and INTEGER-valued numbers (the tracer renders numbers as PICO-8's
-- `printh` does; fractions go through `__hex`), and `__hex` at the range's
-- ends.

printh("string")
printh("")
printh("with spaces and 0x2e.")
printh(true)
printh(false)

printh(0)
printh(1)
printh(-1)
printh(9)
printh(10)
printh(255)
printh(32767)
printh(-32767)
-- The bottom of the whole-part range, reached by arithmetic rather than by
-- the literal `-32768` (num_literals.lua): this corpus is about formatting.
printh(-32767 - 1)

__hex(0)
__hex(1)
__hex(-1)
__hex(32767)
__hex(-32767 - 1)
