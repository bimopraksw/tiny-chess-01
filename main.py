import json as j,os,urllib.request as u
def P(p,b):return j.load(u.urlopen(os.environ["MAKEFUN_ENV_URL"]+p,j.dumps(b).encode()))
V=[0,100,320,330,500,900,0]
N=[-33,-31,-18,-14,14,18,31,33]
W=[-17,-15,15,17,-16,-1,1,16]
M={0:13,4:12,7:14,112:7,116:3,119:11}
def F(f):
 p=f.split();b=[0]*128;r=7;c=0
 for ch in p[0]:
  if ch=='/':r-=1;c=0
  elif ch.isdigit():c+=int(ch)
  else:v="PNBRQK".find(ch.upper())+1;b[r*16+c]=v*(ch<'a' or -1);c+=1
 t=1 if p[1]=='w' else -1;k=sum(1<<"KQkq".find(c) for c in p[2] if c in"KQkq")
 e=-1
 if p[3]!='-':e=ord(p[3][0])-97+(int(p[3][1])-1)*16
 return b,t,k,e
def A(b,s,x):
 for d in (15,17):
  y=s-d*x
  if not y&0x88 and b[y]==x:return 1
 for d in N:
  y=s+d
  if not y&0x88 and b[y]==2*x:return 1
 for d in W:
  y=s+d
  if not y&0x88 and b[y]==6*x:return 1
 for d,w in ((-17,3),(-15,3),(15,3),(17,3),(-16,4),(-1,4),(1,4),(16,4)):
  y=s+d
  while not y&0x88:
   v=b[y]
   if v:
    if v in (w*x,5*x):return 1
    break
   y+=d
 return 0
def G(b,t,k,e):
 m=[]
 for s in range(128):
  if s&0x88:continue
  v=b[s]
  if v*t<=0:continue
  p=abs(v)
  if p==1:
   d=16*t
   for y in (s+d,s+d-1,s+d+1):
    if y&0x88:continue
    z=y==s+d
    if z and b[y]:continue
    if not z and not (b[y]*t<0 or y==e):continue
    if y>>4 in (0,7):
     for q in (5,4,3,2):m.append((s,y,q))
    else:
     m.append((s,y,0))
     if z and (s>>4)==(1 if t>0 else 6) and not b[s+2*d]:m.append((s,s+2*d,0))
  elif p==2 or p==6:
   for d in (N if p==2 else W):
    y=s+d
    if not y&0x88 and b[y]*t<=0:m.append((s,y,0))
  else:
   for d in (W[:4] if p==3 else W[4:] if p==4 else W):
    y=s+d
    while not y&0x88:
     if b[y]:
      if b[y]*t<0:m.append((s,y,0))
      break
     m.append((s,y,0));y+=d
 ks=4 if t>0 else 116
 if k&(1 if t>0 else 4) and b[ks]==6*t and b[ks+3]==4*t and not b[ks+1] and not b[ks+2] and not(A(b,ks,-t)or A(b,ks+1,-t)or A(b,ks+2,-t)):m.append((ks,ks+2,0))
 if k&(2 if t>0 else 8) and b[ks]==6*t and b[ks-4]==4*t and not b[ks-1] and not b[ks-2] and not b[ks-3] and not(A(b,ks,-t)or A(b,ks-1,-t)or A(b,ks-2,-t)):m.append((ks,ks-2,0))
 return m
def R(b,t,k,e,f,y,q):
 c=b[:];v=c[f];c[f]=0
 if abs(v)==1 and y==e:c[y-16*t]=0
 c[y]=q*t if q else v
 if abs(v)==6 and abs(y-f)==2:
  a=(f//16)*16+(7 if y>f else 0);c[a]=0;c[(f+y)//2]=4*t
 return c,k&M.get(f,15)&M.get(y,15),f+16*t if abs(v)==1 and abs(y-f)==32 else -1
def E(b,t):
 s=0
 for r in range(8):
  for f in range(8):
   v=b[r*16+f]
   if not v:continue
   w=1 if v>0 else -1;p=abs(v);rr=r if w>0 else 7-r;z=14-(abs(7-2*f)+abs(7-2*rr));o=V[p]
   if p==1:o+=10*rr+6-2*abs(3-f)
   elif p==2:o+=z*2
   elif p==3:o+=z
   elif p==4:o+=12 if rr==6 else z//2
   elif p==5:o+=z//2
   s+=w*o
 return s*t
C=[0]
def S(b,t,k,e,km,ko,d,a,be,pl):
 C[0]+=1
 if C[0]>2e4 or d==0:return Q(b,t,k,e,km,ko,a,be,0)
 ms=G(b,t,k,e);ms.sort(key=lambda m:t*b[m[1]]);r=-999999;mv=0
 for f,y,q in ms:
  c,k2,e2=R(b,t,k,e,f,y,q);n=y if abs(b[f])==6 else km
  if A(c,n,-t):continue
  mv=1;sc=-S(c,-t,k2,e2,ko,n,d-1,-be,-a,pl+1)
  if sc>r:r=sc
  if sc>a:a=sc
  if a>=be:break
 if not mv:return -100000+pl if A(b,km,-t) else 0
 return r
def Q(b,t,k,e,km,ko,a,be,q):
 st=E(b,t)
 if st>=be:return be
 if st>a:a=st
 if q>5:return a
 ms=[m for m in G(b,t,k,e) if b[m[1]] or m[2] or m[1]==e]
 ms.sort(key=lambda m:t*b[m[1]])
 for f,y,q2 in ms:
  c,k2,e2=R(b,t,k,e,f,y,q2);n=y if abs(b[f])==6 else km
  if A(c,n,-t):continue
  sc=-Q(c,-t,k2,e2,ko,n,-be,-a,q+1)
  if sc>a:a=sc
  if a>=be:break
 return a
def H(fen,legal):
 b,t,k,e=F(fen);km=ko=-1
 for s in range(128):
  if b[s]==6*t:km=s
  elif b[s]==-6*t:ko=s
 C[0]=0;ba=-999999;bm=legal[0]
 L=[]
 for u in legal:
  f=(ord(u[0])-97)+(int(u[1])-1)*16;y=(ord(u[2])-97)+(int(u[3])-1)*16;L.append((u,f,y,"nbrq".find(u[4])+2 if len(u)==5 else 0))
 L.sort(key=lambda z:(t*b[z[2]],z[0]))
 for u,f,y,q in L:
  c,k2,e2=R(b,t,k,e,f,y,q);n=y if abs(b[f])==6 else km
  sc=-S(c,-t,k2,e2,ko,n,3,-999999,-ba,1)
  if sc>ba:ba=sc;bm=u
 return bm
while True:
 g=P("/game",{})
 if g["status"]=="done":break
 while g["status"]=="your_move":g=P("/move",{"game":g["game"],"move":H(g["fen"],g["legal_moves"])})
