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
  b,c:String;
Begin;
if (xtOpen(t,a,true)<>0) then immErr('error opening config!');
ctrlPrt:=BVal(gnl);
dataPrt:=BVal(gnl);
apnName:=gnl;
ConnectionNum:=0;
fillchar(d,sizeof(d),0);
BugOS_MyProcessInfo(o,i,i);
c:=BStr(o);
while not xtEOF(t) do begin;
  b:=gnl;
  if (b='') then continue;
  a:=kicsi(getWord(b));
  if (a='ppp') then begin;
    fillchar(d.addr,sizeof(d.addr),0);
    d.mode:=1;
    end else begin;
    if string2ipAddr(a,d.addr) then continue;
    if isAddressIPv4mask(d.addr) then d.mode:=2 else d.mode:=3;
    end;
  if ResizeMem(ConnectionNum+1) then immErr('error allocating memory!');
  i:=pos('`',b);
  if (i<>0) then b:=copy(b,1,i-1)+c+copy(b,i+1,255);
  d.cmnd:=b;
  ConnectionDat^[ConnectionNum]:=d;
  end;
xtClose(t);
End;
