Function pointDist(x1,y1,x2,y2:LongInt):LongInt;
Label f1;
Var p:LongInt;
Begin;
x1:=abs(x2-x1);
y1:=abs(y2-y1);
p:=0;
while (x1>$4000) or (y1>$4000) do begin;
  x1:=x1 shr 1;
  y1:=y1 shr 1;
  inc(p);
  end;
x2:=(x1*x1)+(y1*y1);
for y2:=31 downto 0 do if ((1 shl y2) and x2<>0) then goto f1;
y2:=0;
f1:
y2:=1 shl (y2 shr 1);
while (y2*y2<x2) do begin;
  inc(y2);
  if (y2>$ffff) then break;
  end;
y2:=y2 shl p;
pointDist:=y2;
End;



Procedure doBasePlacement;
Const beg=1000;
Var
  nodeN:array[1..nodeL] of longint;
  i,o:LongInt;
Begin;
for o:=1 to headR.nodes do nodeN[o]:=0;
for o:=1 to headR.nodes do begin;
  i:=nodeD[o].e-nodeD[o].b+2;
  inc(nodeN[i]);
  nodeD[o].y:=i*20+beg;
  nodeD[o].x:=nodeN[i]*20+beg;
  end;
End;


Function getNodeEnergy(n,x,y:LongInt):LongInt;
Var i,o,p,q:LongInt;
Begin;
q:=0;
for p:=1 to headR.nodes do begin;
  if (p=n) then continue;
  i:=50-pointDist(x,y,nodeD[p].x,nodeD[p].y);
  if (i<0) then continue;
  inc(q,i);
  end;

for o:=nodeD[n].b to nodeD[n].e do begin;
  p:=connD[o].b;
  if (p=n) then continue;
  i:=pointDist(x,y,nodeD[p].x,nodeD[p].y)-100;
  inc(q,abs(i));
  end;

getNodeEnergy:=q;
End;


Function doOneMovement:LongInt;
Const rng=15;
Var
  ox,oy,ov:LongInt;
  cx,cy,cv:LongInt;
  bx,by,bv:LongInt;
  i,o:LongInt;
Begin;
o:=0;
for i:=1 to headR.nodes do begin;
  ox:=nodeD[i].x;
  oy:=nodeD[i].y;
  ov:=getNodeEnergy(i,ox,oy);
  bx:=ox;by:=oy;bv:=ov;
  for cy:=oy-rng to oy+rng do for cx:=ox-rng to ox+rng do begin;
    cv:=getNodeEnergy(i,cx,cy);
    if (bv<=cv) then continue;
    bx:=cx;
    by:=cy;
    bv:=cv;
    end;
  nodeD[i].x:=bx;
  nodeD[i].y:=by;
  inc(o,abs(bx-ox));
  inc(o,abs(by-oy));
  end;
doOneMovement:=o;
End;



Procedure doAllMovement;
Const max=3;
Var
  b:array[1..max] of longint;
  i,o,p:LongInt;
Begin;
doBasePlacement;
p:=0;
repeat
  o:=0;
  for i:=1 to max do inc(o,b[i]);
  i:=doOneMovement;
  p:=(p mod max)+1;
  b[p]:=i;
  inc(o,i);
  until (o=0);
countBoundaries;
End;
