-- `#` here depends on the array part's CAPACITY, which follows from
-- rehash history rather than from the keys. This model answers exactly
-- or raises (`Table::len`), and raises on this. The golden file records
-- what PICO-8 actually says, so the refusal is documented rather than
-- merely asserted.
local u={} u[1]=1 u[2]=2 u[4]=4 printh(#u)
