{$stack 63k}
{$heap 127m}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$define name}

{$include bin.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
halt(1);
End;

{$define dup}
{$undef dup}

{$include repair.inc}



Function decUTF8(a:String):String;
Label f1;
Const charSetIso8859_1:array[0..255] of byte=(0,1,2,3,4,5,6,7,8,9,10,11,
  12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,
  30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,
  48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,
  66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,
  84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,
  102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,
  117,118,119,120,121,122,123,124,125,126,127,46,46,46,46,
  46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,46,
  46,46,46,46,46,46,46,46,46,46,46,33,155,156,46,46,46,
  21,46,46,46,174,46,45,46,46,248,241,253,46,46,230,227,
  249,46,46,46,175,172,171,46,168,46,143,46,46,142,143,46,
  46,46,144,46,46,46,141,46,46,46,46,46,149,46,167,153,
  120,46,46,151,152,154,46,46,46,46,160,131,46,132,134,46,
  46,46,130,136,137,46,161,140,139,46,46,46,162,147,147,
  148,246,149,46,163,150,129,46,46,152);
Var
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  b:String;
  i,o,p,q:LongInt;
Begin;
b:='';
p:=0;
f1:
inc(p);
if (p>ab0) then begin; decUTF8:=b;exit; end;
if (ab[p] and $80=0) then begin;
  b:=b+a[p];
  goto f1;
  end;
o:=-1;
i:=ab[p];
while (i and $80<>0) do begin;
  inc(o);
  i:=i shl 1;
  end;
if (o<1) then goto f1;
if (p+o>ab0) then goto f1;
for i:=1 to o do if (ab[p+i] and $c0<>$80) then goto f1;
q:=((1 shl (7-o))-1) and ab[p];
for i:=1 to o do begin;
  inc(p);
  q:=(q shl 6)+(ab[p] and $7f);
  end;
if (q>255) then begin;
  b:=b+'?';
  goto f1;
  end;
b:=b+chr(charSetIso8859_1[q]);
goto f1;
End;


Function getNextWord(var a:String;s:String):String;
Var i:LongInt;
Begin;
if (s='') then s:=' ';
i:=pos(s,a);
if (i<1) then i:=666;
getNextWord:=copy(a,1,i-1);
a:=copy(a,i+length(s),666);
End;

Function testTagBeg(a,b:String):Boolean;
Begin;
testTagBeg:=(pos('<'+b+' ',a)<>0);
End;

Function testTagEnd(a,b:String):Boolean;
Begin;
testTagEnd:=(pos('</'+b+'>',a)<>0);
End;

Function testClosed(a:String):Boolean;
Begin;
testClosed:=(pos('/>',a)<>0);
End;


Function getTagVal(a,b:String):String;
Var i,o:LongInt;
Begin;
getTagVal:='';
i:=pos(' '+b+'=',a);
if (i<1) then exit;
a:=copy(a,i+length(b)+2,666);
if (copy(a,1,1)='"') then begin;
  a:=copy(a,2,666);
  a:=copy(a,1,pos('"',a)-1);
  end else begin;
  while (copy(a,1,1)=' ') do a:=copy(a,2,666);
  i:=pos(' ',a);
  if (i<1) then i:=666;
  o:=pos('>',a);
  if (o<1) then o:=666;
  if (i>o) then i:=o;
  a:=copy(a,1,i-1);
  end;
getTagVal:=a;
End;


Function getCoord(a:String):LongInt;
Const max=7;
Var i,o:LongInt;
Begin;
i:=pos('.',a);
if (i<1) then i:=666;
o:=BVal(copy(a,1,i-1))*10000000;
a:=copy(a,i+1,max);
while (length(a)<max) do a:=a+'0';
getCoord:=BVal(a)+o;
End;



Procedure readOneNode(var t:xtText);
Var
  d:oneNodeRecord;
  a,b:String;
  i:LongInt;
Begin;
a:=xtReadLn(t,666);
if not testTagBeg(a,'node') then exit;
fillchar(d,sizeof(d),0);
b:=getTagVal(a,'id');
d.x:=getCoord(getTagVal(a,'lon'));
d.y:=getCoord(getTagVal(a,'lat'));
if testClosed(a) then begin;
  addOneNode(b,d);
  exit;
  end;
while not xtEOF(t) do begin;
  a:=xtReadLn(t,666);
  if testTagEnd(a,'node') then break;
  if not testTagBeg(a,'tag') then continue;
  if (getTagVal(a,'k')<>'name') then continue;
  a:=getTagVal(a,'v');
  a:=decUTF8(a);
  if (a='') then continue;
  d.nam:=addOneName(a,nodeN+1,0);
  end;
addOneNode(b,d);
End;




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






Procedure readOneWay(var t:xtText);

function findConn(a,b:LongInt):LongInt;
label f1;
var i:LongInt;
begin;
for i:=connN downto 1 do if (connD[i].a=a) and (connD[i].b=b) then goto f1;
i:=0;
f1:
findConn:=i;
end;

Const pntL=2*1024;
Var
  pntD:array[1..pntL] of LongInt;
  pntN:LongInt;
  oneW:Boolean;
  d:oneConnRecord;
  i,o:LongInt;
  a,b:String;
Begin;
a:=xtReadLn(t,666);
if not testTagBeg(a,'way') then exit;
if testClosed(a) then exit;
fillchar(d,sizeof(d),0);
oneW:=false;
pntN:=0;
while not xtEOF(t) do begin;
  a:=xtReadLn(t,666);
  if testTagEnd(a,'way') then break;
  if testTagBeg(a,'nd') then begin;
    a:=getTagVal(a,'ref');
    i:=findOneNode(a);
    if (i<1) then immErr('invalid reference!');
    inc(pntN);
    if (pntN>pntL) then immErr(b+' too long way!');
    pntD[pntN]:=i;
    continue;
    end;
  if not testTagBeg(a,'tag') then continue;
  b:=getTagVal(a,'k');
  if (b='oneway') then begin;
    oneW:=true;
    continue;
    end;
  if (b='name') then begin;
    a:=getTagVal(a,'v');
    a:=decUTF8(a);
    if (a='') then continue;
    d.nam:=addOneName(a,0,connN+1);
    continue;
    end;
  end;
for i:=2 to pntN do begin;
  d.a:=pntD[i-1];
  d.b:=pntD[i];
  addOneConn(d);
  end;
if oneW then exit;
for i:=pntN-1 downto 1 do begin;
  d.a:=pntD[i+1];
  d.b:=pntD[i];
  addOneConn(d);
  end;
End;






Var
  t:xtText;
  f:xFile;
  a,b:String;
  i,o:LongInt;
  nd:oneNodeRecord;
  cd:oneConnRecord;
  md:oneNameRecord;
BEGIN;
writeLn('osm2bin v1.0, done by Mc at '#%date' '#%time'.');
a:=paramStr(1);
if (a='') then immErr('using: prog.code <text>');
writeln('reading '+a+'...');
xtOpen(t,a,true);

while not xtEOF(t) do begin;
  a:=xtReadLn(t,666);
  if testTagBeg(a,'bound') then break;
  if testTagBeg(a,'bounds') then break;
  end;
b:=getTagVal(a,'box');
if (b='') then
 b:=getTagVal(a,'minlat')+','+getTagVal(a,'minlon')+','+getTagVal(a,'maxlat')+','+getTagVal(a,'maxlon');

headR.begY:=getCoord(getNextWord(b,','));
headR.begX:=getCoord(getNextWord(b,','));
headR.endY:=getCoord(getNextWord(b,','));
headR.endX:=getCoord(getNextWord(b,','));

nodeN:=0;
nameN:=0;
connN:=0;

writeLn('reading nodes...');
xtSetPos(t,0);
while not xtEOF(t) do begin;
  if (nodeN and $ff=1) then write(Bstr(nodeN)+#13);
  readOneNode(t);
  end;
WriteLn(BStr(nodeN)+' nodes readed.');

WriteLn('sorting nodes...');
sortNodesName;

writeLn('reading ways...');
xtSetPos(t,0);
while not xtEOF(t) do begin;
  if (connN and $ff=1) then write(Bstr(connN)+#13);
  readOneWay(t);
  end;
WriteLn(BStr(connN)+' conns readed.');
WriteLn(BStr(nameN)+' names found.');
xtClose(t);

WriteLn('sorting conns...');
for o:=1 to connN do
 connD[o].s:=pointDist(nodeD[connD[o].a].x,nodeD[connD[o].a].y,nodeD[connD[o].b].x,nodeD[connD[o].b].y);
sortConns;

WriteLn('sorting nodes...');
sortNodesCoord;

WriteLn('sorting names...');
sortNames;

WriteLn('finding node conns...');
findNodeConns;

WriteLn('ordering entries...');
orderEntries;

a:=paramStr(1)+'.bin';
writeln('writing '+a+'...');

xErase(a);
xCreate(a);
xOpen(f,a,xGenFilMod_rw);

WriteLn('writing data...');

writeDownTables(f);
xClose(f);

xtOpen(t,a,false);

WriteLn('writing names...');
for o:=1 to nameN do begin;
  a:=nameS[o];
  xtWrite(t,a);
  xtWrite(t,#0);
  end;

xtClose(t);
END.