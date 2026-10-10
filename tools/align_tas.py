#!/usr/bin/env python3
# Align a community tasdatabase TAS with one of ours, frame by frame, in the ORIGINAL cart.
# usage: tools/align_tas.py ROOM DBNAME(e.g. 2900m) PROLOGUE OUR_TAS_FILE SEEDS|- [OUTDIR]
# env TAS_CATEGORY: the tasdatabase category (default nodiag); REPLAY_ARGS: extra replay.py flags (e.g. --one-dash)
# With OUTDIR, also writes OUTDIR/ours.txt and OUTDIR/reference.txt: the two paths for
# `rewrite export-ui --witness ours.txt --reference reference.txt` (label, inputs, dash
# starts as the cart reports them, the tasdatabase name / prologue / seeds, then `f x y` per frame from 0 to the exit).
import json,os,re,subprocess,sys
D='/home/philippe/src/github.com/CelesteClassic/tasdatabase'
# The checkout this file is in: its replay.py (a branch's --jank, say), not the main tree's.
M=os.path.dirname(os.path.dirname(os.path.abspath(__file__)))
CAT=os.environ.get('TAS_CATEGORY','nodiag')
room,name,off,ours_file,seeds=sys.argv[1:6]
outdir=sys.argv[6] if len(sys.argv)>6 else None
e=[e for e in json.load(open(D+'/database.json'))['classic'][CAT] if e['name']==name][0]
s=open(f"{D}/classic/{CAT}/{e['file']}").read()
tas=[0]*int(off)+[int(x) for x in re.findall(r'\d+',s[s.index(']')+1:])]
ours=[int(x) for x in open(os.path.join(M, ours_file)).read().split('\n') if x and not x.startswith('#') for x in x.split(',')]
def run(seq):
    cmd=[M+'/pico8_diff/replay.py','--lua','/home/philippe/src/github.com/tehwalris/celeste_ocaml/celeste.lua','--begin-game','--room',room,'--inputs',','.join(map(str,seq)),'--frames',str(len(seq)+1)]
    cmd+=os.environ.get('REPLAY_ARGS','').split()
    if seeds!='-': cmd+=['--balloon-seeds',seeds]
    out=subprocess.run(cmd,capture_output=True,text=True,cwd=M,timeout=600).stdout
    rows={}; dash={}
    for l in out.splitlines():
        if not (m:=re.match(r'f(\d+) room (\S+)',l)): continue
        f,r=int(m.group(1)),m.group(2)
        # ` dash DIR`: the cart's own dash branch ran this frame (replay.py)
        if d:=re.search(r' dash (\S+)$',l): dash[f]=d.group(1)
        if m:=re.search(r' player (-?[\d.]+),(-?[\d.]+)',l): rows[f]=(r,float(m.group(1)),float(m.group(2)))
        elif m:=re.search(r' player_spawn (-?[\d.]+),(-?[\d.]+)',l): rows[f]=(r,float(m.group(1)),float(m.group(2)))  # rising from below
        else: rows[f]=(r,None,None)
    return rows,dash
def btn(b): return ''.join(c for c,bit in zip('LRUDJX',[1,2,4,8,16,32]) if b&bit) or '-'
(A,DA),(B,DB)=run(tas),run(ours)
def ex(R):
    # The replay runs one frame past the inputs (a path file holds at most that many);
    # a run that has not left by then is not a route under these seeds.
    out=[f for f,(r,*_) in R.items() if r!=room]
    if not out: sys.exit(f"does not leave room {room} by f{max(R)} (the balloon seeds {seeds}?)")
    return min(out)
print(f"{name}: community exits f{ex(A)}, ours f{ex(B)}")
print(" f   community(input  x,y)        ours(input  x,y)")
for f in range(int(off),max(ex(A),ex(B))+1):
    a=A.get(f,('?',None,None))[:3]; b=B.get(f,('?',None,None))[:3]
    ia=btn(tas[f-1]) if f<=len(tas) else ''; ib=btn(ours[f-1]) if f<=len(ours) else ''
    mark='' if (a[1:]==b[1:] and ia==ib) else ' *'
    pa=f"{a[1]:.0f},{a[2]:.0f}" if a[1] is not None else a[0]; pb=f"{b[1]:.0f},{b[2]:.0f}" if b[1] is not None else b[0]
    print(f"{f:3} {ia:>6} {pa:>9}      {ib:>6} {pb:>9}{mark}")
def dashes(D,end): return [f"{f}:{d}" for f,d in sorted(D.items()) if f<=end]
def write(path,label,seq,R,D):
    end=ex(R)
    with open(path,'w') as o:
        # db / prologue / seeds: the UI's "Download .tas" (the database's format, the community file's seeds)
        o.write(f"label {label}\ninputs {','.join(map(str,seq))}\ndashes {' '.join(dashes(D,end))}\n"
                f"db {name} {CAT}\nprologue {off}\nseeds {s[:s.index(']')+1].strip()}\n0 - -\n")
        for f in range(1,end+1):
            r=R.get(f)
            o.write(f"{f} {r[1]:.0f} {r[2]:.0f}\n" if r and r[0]==room and r[1] is not None else f"{f} - -\n")
    print(f"wrote {path}: {label}, dashes {' '.join(dashes(D,end))}")
if outdir:
    os.makedirs(outdir, exist_ok=True)
    write(f"{outdir}/reference.txt",f"{e['file'][:-4]} (community, {name}, original cart): exits f{ex(A)}",tas,A,DA)
    write(f"{outdir}/ours.txt",f"ours (search witness, original cart): exits f{ex(B)}",ours,B,DB)
