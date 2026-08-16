# Generated from a real console

`sin_table.p8` dumps PICO-8's `sin` for every input it can distinguish, as
exact 16.16 hex. Run it with a real PICO-8 and rebuild `cart/pico8_sin_table.bin`:

    pico8 -x pico8_diff/gen/sin_table.p8 2>/dev/null | grep '^0x' > /tmp/sintable.txt
    # then fold to 16384 entries: see the two facts below.

Two facts, both verified over all 65536 inputs rather than assumed:

* `sin` is constant over blocks of TWO adjacent inputs, so the low bit of the
  16-bit fraction is dead and 32768 entries suffice;
* `sin(x + 0.5) == -sin(x)` EXACTLY, so only the first half-turn is stored.

Together: 16384 little-endian `i32`, indexed by `(frac & 0x7fff) >> 1`,
negated when `frac >= 32768`. Reconstruction was checked against all 65536
console values with zero mismatches.

Note `sin(0.5 - x) == sin(x)` does NOT hold, though real sine says it should -
the console's table is asymmetric under rounding. That is exactly why this is
a dump and not a formula.

The generator loops to 32767, not 65535: 65536 is not representable in 16.16,
so `for i=0,65535` never runs and `i/65536` is garbage. Both mistakes were
made before this comment existed.
