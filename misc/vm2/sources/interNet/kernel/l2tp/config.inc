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
servPort:=BVal(gnl);
servName:=gnl;
ConnectionNum:=0;
fillchar(d,sizeof(d),0);
BugOS_MyProcessInfo(o,i,i);
b:=BStr(o);
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  if ResizeMem(ConnectionNum+1) then immErr('error allocating memory!');
  i:=pos('`',a);
  if (i<>0) then a:=copy(a,1,i-1)+b+copy(a,i+1,255);
  d.cmnd:=a;
  ConnectionDat^[ConnectionNum]:=d;
  end;
xtClose(t);
if (servName='') then if (xtOpen(t,'c:\system\localHost.text',true)=0) then begin;
  servName:=xtReadLn(t,666);
  xtClose(t);
  end;
if (servName='') then servName:='localHost';
End;
