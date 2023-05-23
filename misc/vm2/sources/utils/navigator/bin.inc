Const
  nodeL=768*1024;
  connL=4*nodeL;
  nameL=nodeL shr 4;
Const fileIDvalue=$50414d62;
Type
  oneHeaderRecord=record
    id:LongInt;
    begX:LongInt;
    begY:LongInt;
    endX:LongInt;
    endY:LongInt;
    nodes:LongInt;
    conns:LongInt;
    names:LongInt;
    end;
  oneNodeRecord=record
    x:LongInt;
    y:LongInt;
    b:LongInt;
    e:LongInt;
    nam:LongInt;
    end;
  oneConnRecord=record
    a:LongInt;
    b:LongInt;
    s:LongInt;
    nam:LongInt;
    end;
  oneNameRecord=record
    ofs:LongInt;
    nod:LongInt;
    con:LongInt;
    end;
Var
  headR:oneHeaderRecord;
  nodeD:array[1..nodeL] of oneNodeRecord;
  connD:array[1..connL] of oneConnRecord;
{$ifdef name}
  nameD:array[1..nameL] of oneNameRecord;
  nameP:LongInt;
{$endif}



Function readUpTables(var f:xFile):Boolean;
Const maxBuf=32*1024;
Var fp,fs:LongInt;

procedure doer(var buf;siz:LongInt);
var
  b:array[1..1] of byte absolute buf;
  i,o,p:LongInt;
begin;
p:=1;
while (siz>0) do begin;
  i:=siz;
  o:=maxBuf-(fp mod maxBuf);
  if (i>o) then i:=o;
  xBlockRead(f,b[p],i);
  inc(fp,i);
  dec(siz,i);
  inc(p,i);
  end;
end;

Begin;
readUpTables:=True;
fs:=xFileSize(f);
if (fs<sizeof(headR)) then exit;
xSeek(f,0);
xBlockRead(f,headR,sizeof(headR));
if (headR.id<>fileIDvalue) then exit;
fp:=sizeof(headR);
doer(nodeD,headR.nodes*sizeof(oneNodeRecord));
doer(connD,headR.conns*sizeof(oneConnRecord));
{$ifdef name}
doer(nameD,headR.names*sizeof(oneNameRecord));
nameP:=xFilePos(f);
{$endif}
readUpTables:=False;
End;





Function writeDownTables(var f:xFile):Boolean;
Const maxBuf=32*1024;
Var fp:LongInt;

procedure doer(var buf;siz:LongInt);
var
  b:array[1..1] of byte absolute buf;
  i,o,p:LongInt;
begin;
p:=1;
while (siz>0) do begin;
  i:=siz;
  o:=maxBuf-(fp mod maxBuf);
  if (i>o) then i:=o;
  xBlockWrite(f,b[p],i);
  inc(fp,i);
  dec(siz,i);
  inc(p,i);
  end;
end;

Begin;
writeDownTables:=True;
xSeek(f,0);
xBlockWrite(f,headR,sizeof(headR));
fp:=sizeof(headR);
doer(nodeD,headR.nodes*sizeof(oneNodeRecord));
doer(connD,headR.conns*sizeof(oneConnRecord));
{$ifdef name}
doer(nameD,headR.names*sizeof(oneNameRecord));
nameP:=xFilePos(f);
{$endif}
writeDownTables:=False;
End;





{$ifdef name}
Function readOneName(var f:xFile;p:LongInt):String;
Const max=200;
Var
  i,o,s:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
Begin;
readOneName:='';
if (p<1) then exit;
if (p>=headR.names) then exit;
s:=xFileSize(f)-nameP;
i:=nameD[p].ofs;
xSeek(f,i+nameP);
i:=s-i;
if (i>max) then i:=max;
xBlockRead(f,a[1],i);
p:=i;
for i:=i downto 1 do if (ab[i]=0) then p:=i;
ab[0]:=p-1;
readOneName:=a;
End;



Function findOneName(var f:xFile;c:String):LongInt;
Var
  b,e,p,s:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
Begin;
c:=kicsi(c);
b:=1;
e:=headR.names;
while (b+1<e) do begin;
  p:=((e-b) shr 1)+b;
  a:=kicsi(readOneName(f,p));
  if (c<a) then e:=p else b:=p;
  end;
dec(p);
while (p<headR.names) do begin;
  inc(p);
  a:=kicsi(readOneName(f,p));
  if (c>a) then continue;
  if (c+#255<a) then dec(p);
  break;
  end;
findOneName:=p;
End;



Function getNameNode(o:LongInt):LongInt;
Var i:LongInt;
Begin;
if (o>0) then i:=nameD[o].con else i:=0;
if (i>0) then i:=connD[i].a else i:=nameD[o].nod;
getNameNode:=i;
End;
{$endif}



Function getNodeName(o:LongInt):LongInt;
Label f1;
Var i,p:LongInt;
Begin;
p:=nodeD[o].nam;
if (p<>0) then goto f1;
for i:=nodeD[o].b to nodeD[o].e do begin;
  p:=connD[i].nam;
  if (p<>0) then goto f1;
  end;
p:=0;
f1:
getNodeName:=p;
End;





Function findNearestNode(x,y:LongInt;nam,con:LongInt):LongInt;
Var i,o,p,q:LongInt;
Begin;
dec(con);
q:=0;
p:=999999999;
for i:=1 to headR.nodes do begin;
  if (nam<>0) then if (getNodeName(i)=0) then continue;
  if (nodeD[i].e-nodeD[i].b<con) then continue;
  o:=abs(nodeD[i].x-x)+abs(nodeD[i].y-y);
  if (o>p) then continue;
  p:=o;
  q:=i;
  end;
findNearestNode:=q;
End;
