-- `#` here depends on the array part's CAPACITY, which follows from
-- rehash history rather than from the keys. This model answers exactly
-- or raises (`Table::len`), and raises on this. The golden file records
-- what PICO-8 actually says, so the refusal is documented rather than
-- merely asserted.
local n={} n[1]={} n[1][2]="deep" printh(#n[1])
