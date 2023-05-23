Var
  nameI:array[1..nameL] of LongInt;
  nodeI:array[1..nodeL] of LongInt;
  connI:array[1..connL] of LongInt;
  nameR:array[1..nameL] of LongInt;
  nodeR:array[1..nodeL] of LongInt;
  connR:array[1..connL] of LongInt;
  nameS:array[1..nameL] of string[63];
  nodeS:array[1..nodeL] of string[31];
  nameN:LongInt;
  nodeN:LongInt;
  connN:LongInt;



Function addOneName(a:String;n,c:LongInt):LongInt;
Var
  d:oneNameRecord;
  i:LongInt;
Begin;
{$ifdef dup}
for i:=nameN downto 1 do if (nameS[i]=a) then begin; addOneName:=i;exit; end;
{$endif}
inc(nameN);
if (nameN>nameL) then immErr('too much names!');
fillchar(d,sizeof(d),0);
if (n<>0) then d.nod:=n;
if (c<>0) then d.con:=c;
nameS[nameN]:=a;
nameD[nameN]:=d;
nameI[nameN]:=nameN;
addOneName:=nameN;
End;



Procedure addOneNode(a:String;var d:oneNodeRecord);
Var i:LongInt;
Begin;
{$ifdef dup}
for i:=nodeN downto 1 do if (nodeS[i]=a) then exit;
{$endif}
inc(nodeN);
if (nodeN>nodeL) then immErr('too much nodes!');
nodeS[nodeN]:=a;
nodeD[nodeN]:=d;
End;



Procedure addOneConn(var d:oneConnRecord);
Var i:LongInt;
Begin;
if (d.a=d.b) then exit;
{$ifdef dup}
for i:=connN downto 1 do if (connD[i].a=d.a) and (connD[i].b=d.b) then exit;
{$endif}
inc(connN);
if (connN>connL) then immErr('too many conns!');
connD[connN]:=d;
End;



Procedure sortNames;

function QuickReadOne(i:LongInt):String;
begin;
QuickReadOne:=kicsi(nameS[nameI[i]]);
end;

procedure QuickSwapOne(a,b:LongInt);
var c:LongInt;
begin;
c:=nameI[a];
nameI[a]:=nameI[b];
nameI[b]:=c;
end;

{$sysinc quicksrt.inc}

Var i,o,p:LongInt;
Begin;
for i:=1 to nameN do nameI[i]:=i;
if (nameN>1) then QuickSort(1,nameN);
for i:=1 to nameN do nameR[nameI[i]]:=i;
End;




Procedure sortNodesName;

function QuickReadOne(i:LongInt):String;
begin;
QuickReadOne:=nodeS[i];
end;

procedure QuickSwapOne(a,b:LongInt);
var
  d:oneNodeRecord;
  s:String;
  c:LongInt;
begin;
d:=nodeD[a];
nodeD[a]:=nodeD[b];
nodeD[b]:=d;
s:=nodeS[a];
nodeS[a]:=nodeS[b];
nodeS[b]:=s;
c:=nodeI[a];
nodeI[a]:=nodeI[b];
nodeI[b]:=c;
end;

{$sysinc quicksrt.inc}

Var i,o,p:LongInt;
Begin;
for i:=1 to nodeN do nodeI[i]:=i;
if (nodeN>1) then QuickSort(1,nodeN);
for i:=1 to nodeN do nodeR[nodeI[i]]:=i;
for i:=1 to nameN do begin;
  o:=nameD[i].nod;
  if (o>0) then nameD[i].nod:=nodeR[o];
  end;
End;





Function findOneNode(a:String):LongInt;

function QuickReadOne(i:LongInt):String;
begin;
QuickReadOne:=nodeS[i];
end;

procedure QuickSwapOne(a,b:LongInt);
begin;
end;

{$sysinc quicksrt.inc}

Begin;
findOneNode:=QuickFind(1,nodeN,a);
End;





Procedure sortNodesCoord;

function QuickReadOne(i:LongInt):String;
var
  d:oneNodeRecord;
  a:string;
  ab0:byte absolute a;
begin;
d:=nodeD[nodeI[i]];
move(d.x,a[1],sizeof(i));
move(d.y,a[sizeof(i)+1],sizeof(i));
ab0:=2*sizeof(i);
QuickReadOne:=a;
end;

procedure QuickSwapOne(a,b:LongInt);
var c:LongInt;
begin;
c:=nodeI[a];
nodeI[a]:=nodeI[b];
nodeI[b]:=c;
end;

{$sysinc quicksrt.inc}

Var i,o,p:LongInt;
Begin;
for i:=1 to nodeN do nodeI[i]:=i;
if (nodeN>1) then QuickSort(1,nodeN);
for i:=1 to nodeN do nodeR[nodeI[i]]:=i;
End;





Procedure findNodeConns;

function QuickReadOne(i:LongInt):String;
var
  a:string;
  ab0:byte absolute a;
begin;
move(connD[connI[i]].a,a[1],sizeof(i));
ab0:=sizeof(i);
QuickReadOne:=a;
end;

procedure QuickSwapOne(a,b:LongInt);
begin;
end;

{$sysinc quicksrt.inc}

Label f1;
Var
  i,o,p,q:LongInt;
  a:String;
  ab0:byte absolute a;
Begin;
for q:=1 to nodeN do begin;
  move(q,a[1],sizeof(i));
  ab0:=sizeof(i);
  p:=QuickFind(1,connN,a);
  if (p<1) then begin; o:=0;p:=-1;goto f1; end;
  o:=p;
  while (o>1) do begin;
    if (connD[connI[o-1]].a<>q) then break;
    dec(o);
    end;
  while (p<connN) do begin;
    if (connD[connI[p+1]].a<>q) then break;
    inc(p);
    end;
  f1:
  nodeD[q].b:=o;
  nodeD[q].e:=p;
  end;
End;





Procedure sortConns;

function QuickReadOne(i:LongInt):String;
var
  d:oneConnRecord;
  a:string;
  ab0:byte absolute a;
begin;
d:=connD[connI[i]];
move(d.a,a[1],sizeof(i));
move(d.b,a[sizeof(i)+1],sizeof(i));
ab0:=2*sizeof(i);
QuickReadOne:=a;
end;

procedure QuickSwapOne(a,b:LongInt);
var c:LongInt;
begin;
c:=connI[a];
connI[a]:=connI[b];
connI[b]:=c;
end;

{$sysinc quicksrt.inc}

Var i,o:LongInt;
Begin;
for i:=1 to connN do connI[i]:=i;
if (connN>1) then QuickSort(1,connN);
for i:=1 to connN do connR[connI[i]]:=i;
End;






Procedure countBoundaries;
Var i,o,p:LongInt;
Begin;
headR.begX:=nodeD[1].x;
headR.begY:=nodeD[1].y;
headR.endX:=headR.begX;
headR.endY:=headR.begY;
for p:=1 to headR.nodes do begin;
  i:=nodeD[p].x;
  o:=nodeD[p].y;
  if (i<headR.begX) then headR.begX:=i;
  if (o<headR.begY) then headR.begY:=o;
  if (i>headR.endX) then headR.endX:=i;
  if (o>headR.endY) then headR.endY:=o;
  end;
End;





Procedure orderEntries;
Var
  i,o:LongInt;
  nd:oneNodeRecord;
  cd:oneConnRecord;
  md:oneNameRecord;
  st:String;

procedure swapInt(var a,b:LongInt);
var c:LongInt;
begin;
c:=a;
a:=b;
b:=c;
end;

Begin;
headR.id:=fileIDvalue;
headR.nodes:=nodeN;
headR.conns:=connN;
headR.names:=nameN;

for o:=1 to nodeN do begin;
  nd:=nodeD[o];
  i:=nd.nam;
  if (i>0) then i:=nameR[i];
  nd.nam:=i;
  i:=nodeI[o];
  nodeD[o]:=nd;
  end;

for o:=1 to connN do begin;
  cd:=connD[o];
  cd.a:=nodeR[cd.a];
  cd.b:=nodeR[cd.b];
  i:=cd.nam;
  if (i>0) then i:=nameR[i];
  cd.nam:=i;
  i:=connI[o];
  connD[o]:=cd;
  end;

for o:=1 to nameN do begin;
  md:=nameD[o];
  i:=md.nod;
  if (i<>0) then i:=nodeR[i];
  md.nod:=i;
  i:=md.con;
  if (i<>0) then i:=connR[i];
  md.con:=i;
  nameD[o]:=md;
  end;

for o:=1 to nodeN do begin;
  i:=nodeI[o];
  nd:=nodeD[o];
  nodeD[o]:=nodeD[i];
  nodeD[i]:=nd;
  swapInt(nodeI[o],nodeI[nodeR[o]]);
  swapInt(nodeR[i],nodeR[o]);
  end;

for o:=1 to connN do begin;
  i:=connI[o];
  cd:=connD[o];
  connD[o]:=connD[i];
  connD[i]:=cd;
  swapInt(connI[o],connI[connR[o]]);
  swapInt(connR[i],connR[o]);
  end;

for o:=1 to nameN do begin;
  i:=nameI[o];
  md:=nameD[o];
  nameD[o]:=nameD[i];
  nameD[i]:=md;
  st:=nameS[o];
  nameS[o]:=nameS[i];
  nameS[i]:=st;
  swapInt(nameI[o],nameI[nameR[o]]);
  swapInt(nameR[i],nameR[o]);
  end;

o:=0;
for i:=1 to nameN do begin;
  nameD[i].ofs:=o;
  inc(o,length(nameS[i]));
  inc(o);
  end;

End;
