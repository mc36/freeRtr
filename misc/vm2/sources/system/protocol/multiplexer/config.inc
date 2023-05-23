Procedure readUpConfig(a:String);
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


Var
  d:OneConnectionRecord;
  i,o:LongInt;
  b:String;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
fillchar(d,sizeof(d),0);
ResizeMem(2);
d.pipe:=0;
d.name:='';
ConnectionDat^[1]:=d;
ConnectionDat^[2]:=d;
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  i:=pos(' ',a);
  d.name:=kicsi(copy(a,i+1,255));
  a:=copy(a,1,i-1);
  i:=pos('-',a);
  b:=copy(a,1,i-1);
  a:=copy(a,i+1,255);
  if (i<1) then b:=a;
  i:=BVal(b);
  o:=BVal(a);
  d.protB:=i;
  d.protE:=o;
  if (i=0) and (o=$ffff) then begin; ConnectionDat^[1]:=d;continue; end;
  if (i=$ffff) and (o=0) then begin; ConnectionDat^[2]:=d;continue; end;
  if (i=0) and (o=0) then continue;
  if (i>o) then continue;
  if ResizeMem(ConnectionNum+1) then immErr('error allocating memory!');
  ConnectionDat^[ConnectionNum]:=d;
  end;
xtClose(t);
End;
