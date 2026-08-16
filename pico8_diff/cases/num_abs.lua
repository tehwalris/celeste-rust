-- `abs` away from the range boundary. `abs(-32768)` is a known divergence
-- and lives in num_abs_min.lua so it cannot mask these.

__hex(abs(0))
__hex(abs(1))
__hex(abs(-1))
__hex(abs(0.5))
__hex(abs(-0.5))
__hex(abs(0.00002))
__hex(abs(-0.00002))
__hex(abs(255.5))
__hex(abs(-255.5))
__hex(abs(32767))
__hex(abs(-32767))
__hex(abs(1 / 3))
__hex(abs(-1 / 3))
