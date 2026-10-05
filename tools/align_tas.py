#!/usr/bin/env python3
# Align a community tasdatabase TAS with one of ours, frame by frame, in the ORIGINAL cart.
# usage: tools/align_tas.py ROOM DBNAME(e.g. 2900m) PROLOGUE OUR_TAS_FILE SEEDS|- [OUTDIR]   (category: classic/nodiag)
# With OUTDIR, also writes OUTDIR/ours.txt and OUTDIR/reference.txt: the two paths for
# `rewrite export-ui --witness ours.txt --reference reference.txt` (label, inputs, dash
# starts, then `f x y` per frame from 0 to the exit).
import json,re,subprocess,sys
D='/home/philippe/src/github.com/CelesteClassic/tasdatabase'
M='/home/philippe/src/github.com/tehwalris/celeste-rust'
room,name,off,ours_file,seeds=sys.argv[1:6]
outdir=sys.argv[6] if len(sys.argv)>6 else None
e=[e for e in json.load(open(D+'/database.json'))['classic']['nodiag'] if e['name']==name][0]
s=open(f"{D}/classic/nodiag/{e['file']}").read()
tas=[0]*int(off)+[int(x) for x in re.findall(r'\d+',s[s.index(']')+1:])]
ours=[int(x) for x in open(f"{M}/{ours_file}").read().split('\n') if x and not x.startswith('#') for x in x.split(',')]
def fix(h):  # tostr(v, true): 16.16 fixed point in hex, 0xiiii.ffff
    v=int(h.replace('0x','').replace('.',''),16)
    return (v-(1<<32) if v>=1<<31 else v)/65536
def run(seq):
    cmd=[M+'/pico8_diff/replay.py','--lua','/home/philippe/src/github.com/tehwalris/celeste_ocaml/celeste.lua','--begin-game','--room',room,'--inputs',','.join(map(str,seq)),'--frames',str(len(seq)+1)]
    if seeds!='-': cmd+=['--balloon-seeds',seeds]
    out=subprocess.run(cmd,capture_output=True,text=True,cwd=M,timeout=600).stdout
    rows={}
    for l in out.splitlines():
        m=re.match(r'f(\d+) room (\S+) .*?player (-?[\d.]+),(-?[\d.]+)(?: spd (\S+),(\S+))?',l)
        if m: rows[int(m.group(1))]=(m.group(2),float(m.group(3)),float(m.group(4)))+((fix(m.group(5)),fix(m.group(6))) if m.group(5) else (None,None))
        elif m:=re.match(r'f(\d+) room (\S+) .*?player_spawn (-?[\d.]+),(-?[\d.]+)',l): rows[int(m.group(1))]=(m.group(2),float(m.group(3)),float(m.group(4)),None,None)  # rising from below
        elif re.match(r'f(\d+) room',l): rows[int(l.split()[0][1:])]=(l.split()[2],None,None,None,None)
    return rows
def btn(b): return ''.join(c for c,bit in zip('LRUDJX',[1,2,4,8,16,32]) if b&bit) or '-'
A,B=run(tas),run(ours)
ex=lambda R: min(f for f,(r,*_) in R.items() if r!=room)
print(f"{name}: community exits f{ex(A)}, ours f{ex(B)}")
print(" f   community(input  x,y)        ours(input  x,y)")
for f in range(int(off),max(ex(A),ex(B))+1):
    a=A.get(f,('?',None,None))[:3]; b=B.get(f,('?',None,None))[:3]
    ia=btn(tas[f-1]) if f<=len(tas) else ''; ib=btn(ours[f-1]) if f<=len(ours) else ''
    mark='' if (a[1:]==b[1:] and ia==ib) else ' *'
    pa=f"{a[1]:.0f},{a[2]:.0f}" if a[1] is not None else a[0]; pb=f"{b[1]:.0f},{b[2]:.0f}" if b[1] is not None else b[0]
    print(f"{f:3} {ia:>6} {pa:>9}      {ib:>6} {pb:>9}{mark}")
def dashes(R,end):
    # A dash sets the speed to 5 px/frame along its axis (d_full); nothing else reaches 4.5.
    out=[]; was=False
    for f in range(1,end+1):
        sx,sy=(R.get(f,(None,)*5)[3:] or (None,None))
        d=''.join(c for c,v in (('R',sx),('L',sx and -sx),('D',sy),('U',sy and -sy)) if v is not None and v>=4.5)
        if d and not was: out.append(f"{f}:{d}")
        was=bool(d)
    return out
def write(path,label,seq,R):
    end=ex(R)
    with open(path,'w') as o:
        o.write(f"label {label}\ninputs {','.join(map(str,seq))}\ndashes {' '.join(dashes(R,end))}\n0 - -\n")
        for f in range(1,end+1):
            r=R.get(f)
            o.write(f"{f} {r[1]:.0f} {r[2]:.0f}\n" if r and r[0]==room and r[1] is not None else f"{f} - -\n")
    print(f"wrote {path}: {label}, dashes {' '.join(dashes(R,end))}")
if outdir:
    write(f"{outdir}/reference.txt",f"{e['file'][:-4]} (community, {name}, original cart): exits f{ex(A)}",tas,A)
    write(f"{outdir}/ours.txt",f"ours (search witness, original cart): exits f{ex(B)}",ours,B)
