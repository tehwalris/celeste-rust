#!/usr/bin/env python3
# Align a community tasdatabase TAS with one of ours, frame by frame, in the ORIGINAL cart.
# usage: tools/align_tas.py ROOM DBNAME PROLOGUE OUR_TAS_FILE SEEDS|-   (category: classic/nodiag)
import json,re,subprocess,sys
D='/home/philippe/src/github.com/CelesteClassic/tasdatabase'
M='/home/philippe/src/github.com/tehwalris/celeste-rust'
room,name,off,ours_file,seeds=sys.argv[1:6]
e=[e for e in json.load(open(D+'/database.json'))['classic']['nodiag'] if e['name']==name][0]
s=open(f"{D}/classic/nodiag/{e['file']}").read()
tas=[0]*int(off)+[int(x) for x in re.findall(r'\d+',s[s.index(']')+1:])]
ours=[int(x) for x in open(f"{M}/{ours_file}").read().split('\n') if x and not x.startswith('#') for x in x.split(',')]
def run(seq):
    cmd=[M+'/pico8_diff/replay.py','--lua','/home/philippe/src/github.com/tehwalris/celeste_ocaml/celeste.lua','--begin-game','--room',room,'--inputs',','.join(map(str,seq)),'--frames',str(len(seq)+1)]
    if seeds!='-': cmd+=['--balloon-seeds',seeds]
    out=subprocess.run(cmd,capture_output=True,text=True,cwd=M,timeout=600).stdout
    rows={}
    for l in out.splitlines():
        m=re.match(r'f(\d+) room (\S+) .*?player (-?[\d.]+),(-?[\d.]+)(?: spd (\S+),(\S+))?',l)
        if m: rows[int(m.group(1))]=(m.group(2),float(m.group(3)),float(m.group(4)))
        elif re.match(r'f(\d+) room',l): rows[int(l.split()[0][1:])]=(l.split()[2],None,None)
    return rows
def btn(b): return ''.join(c for c,bit in zip('LRUDJX',[1,2,4,8,16,32]) if b&bit) or '-'
A,B=run(tas),run(ours)
ex=lambda R: min(f for f,(r,_,_) in R.items() if r!=room)
print(f"{name}: community exits f{ex(A)}, ours f{ex(B)}")
print(" f   community(input  x,y)        ours(input  x,y)")
for f in range(int(off),max(ex(A),ex(B))+1):
    a=A.get(f,('?',None,None)); b=B.get(f,('?',None,None))
    ia=btn(tas[f-1]) if f<=len(tas) else ''; ib=btn(ours[f-1]) if f<=len(ours) else ''
    mark='' if (a[1:]==b[1:] and ia==ib) else ' *'
    pa=f"{a[1]:.0f},{a[2]:.0f}" if a[1] is not None else a[0]; pb=f"{b[1]:.0f},{b[2]:.0f}" if b[1] is not None else b[0]
    print(f"{f:3} {ia:>6} {pa:>9}      {ib:>6} {pb:>9}{mark}")
