-- num_literals.lua's literal past the 16.16 range: PICO-8 WRAPS the whole
-- part (32768 reads as -32768), and `-32768` is the negation of that literal
-- (Lua has no negative literals), which wraps back to -32768. The
-- interpreter RAISES on each (src/trace/probe.rs, OVERFLOW).

__hex(32768)
__hex(-32768)
