Procedure ReadUpConfig(a:String);
Var t:xtText;

function gnl:string;
var
  i:longint;
  a:String;
begin;
a:=xtReadLn(t,255);
i:=pos(';',a);
if (i>0) then a:=copy(a,1,i-1);
a:=' '+a+' ';
kicserel('  ',' ',a);
a:=copy(a,2,length(a)-2);
gnl:=a;
end;

function gnw(var b:String):string;
var
  bb:array[0..1] of byte absolute b;
  bb0:byte absolute b;
  a:string;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
  i:longint;
begin;
ab0:=0;
while (bb0>0) do begin;
  if (bb[1]=32) then break;
  inc(ab0);
  ab[ab0]:=bb[1];
  b:=copy(b,2,666);
  end;
b:=copy(b,2,666);
gnw:=a;
end;

Var
  d:OneConnectionRecord;
  b:String;
  i,o:LongInt;
Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
ipProto:=BVal(gnl);
ConnectionNum:=0;
while not xtEOF(t) do begin;
  b:=gnl;
  if (b='') then continue;
  fillchar(d,sizeof(d),0);
  if string2ipAddr(b,d.addr) then continue;
  inc(ConnectionNum);
  ConnectionDat[ConnectionNum]:=d;
  end;
xtClose(t);
End;
