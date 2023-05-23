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

Var
  d:OneConnectionRecord;
  i,o:LongInt;
  b:String;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
ConnectionNum:=0;
servPort:=BVal(gnl);
while not xtEOF(t) do begin;
  a:=gnl;
  fillchar(d,sizeof(d),0);
  b:=getWord(a);
  o:=BVal(b);
  if (o=0) then o:=BugOS_findProcNam(b);
  if (o=0) then continue;
  d.serC:=o;
  d.serD:=BVal(getWord(a));
  if ResizeMem(ConnectionNum+1) then immErr('error allocating memory!');
  ConnectionDat^[ConnectionNum]:=d;
  end;
xtClose(t);
End;
