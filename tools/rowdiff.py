"""Set difference between two checkpoints' visited row keys."""
import sys
sys.path.insert(0, __file__.rsplit('/', 1)[0])
from rowset import keys
a, b = keys(sys.argv[1]), keys(sys.argv[2])
print(f"|A|={len(a)} |B|={len(b)} shared={len(a&b)} onlyA={len(a-b)} onlyB={len(b-a)}")
