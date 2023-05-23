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
  con:OneConnectionRecord;
  b:String;
  i,o:LongInt;
Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
localPort:=BVal(gnl);
ConnectionNum:=0;
while not xtEOF(t) do begin;
  fillchar(con,sizeof(con),0);
  con.port:=5060;
  b:=gnl;
  if (b='') then continue;
  con.name:=kicsi(getWord(b));
  if not string2ipAddr(getWord(b),con.addr) then con.stat:=2;
  con.rout:=kicsi(b);
  inc(ConnectionNum);
  if ResizeMem(ConnectionNum) then immErr('out of memory!');
  ConnectionDat^[ConnectionNum]:=con;
  end;
xtClose(t);
End;
