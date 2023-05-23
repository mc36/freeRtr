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
  myMacAddr:array[1..1] of byte absolute myMACaddress;
  d:OneConnectionRecord;
  i,o:LongInt;
  b:String;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
servPort:=BVal(gnl);
a:=gnl;
Kicserel('.','',a);
Kicserel('-','',a);
Kicserel(' ','',a);
for i:=1 to sizeof(myMACaddress) do begin;
  myMacAddr[i]:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,666);
  end;
ConnectionNum:=0;
CurrentPipeLine:=0;
fillchar(d,sizeof(d),0);
timer2start;
while not xtEOF(t) do begin;
  a:=gnl;
  if (a='') then continue;
  if string2ipAddr(a,d.addr) then continue;
  if ResizeMem(ConnectionNum+1) then immErr('error allocating memory!');
  ConnectionDat^[ConnectionNum]:=d;
  end;
xtClose(t);
End;
