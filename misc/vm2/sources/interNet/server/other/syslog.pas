{$heap 31k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc filesys2.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Var
  serverAddr:OneTCPaddressRecord;
  serverPort:LongInt;
  serverPipe:LongInt;
  logFileName:String;
  logFileDate:LongInt;
  logFileHndr:xtText;
  logFileLock:Boolean;



Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

Function getCurrentDateBin:LongInt;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateBin:=(c shl 16) or (b shl 8) or c;
End;

Function padUpWithZeros(i:LongInt):String;
Var a:String;
Begin;
a:=BStr(i);
while (length(a)<2) do a:='0'+a;
padUpWithZeros:=a;
End;

Function getCurrentDateStr:String;
Var a,b,c:Word;
Begin;
xGetDate(a,b,c);
getCurrentDateStr:=padUpWithZeros(a)+'-'+padUpWithZeros(b)+'-'+padUpWithZeros(c);
End;

Function getCurrentTimeStr:String;
Var a,b,c:Word;
Begin;
xGetTime(a,b,c);
getCurrentTimeStr:=padUpWithZeros(a)+':'+padUpWithZeros(b)+':'+padUpWithZeros(c);
End;


Procedure append2log(act:String;var buffer;size:LongInt);
Const max=250;
Var
  buf:array[1..1] of byte absolute buffer;
  p,i:LongInt;
  a:String;
  ab:array[0..1] of byte absolute a;
  ab0:byte absolute a;
Begin;
while (size>0) and (buf[size] in [13,10]) do dec(size);
i:=getCurrentDateBin;
if (i<>logFileDate) then begin;
  logFileDate:=i;
  xtClose(logFileHndr);
  a:=logFileName;
  i:=pos('%',a);
  if (i>0) then a:=copy(a,1,i-1)+getCurrentDateStr+copy(a,i+1,666);
  xCreate(a);
  if (xtOpen(logFileHndr,a,false)<>0) then immErr('error opening '+a+'!');
  end;
xtWrite(logFileHndr,getCurrentTimeStr+' '+act+' ');
p:=0;
while (p<size) do begin;
  i:=size-p;
  if (i>max) then i:=max;
  ab0:=i;
  move(buf[p+1],ab[1],i);
  inc(p,i);
  xtWrite(logFileHndr,a);
  end;
xtWriteLn(logFileHndr,'');
if logFileLock then exit;
xtClose(logFileHndr);
dec(logFileDate);
End;


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

Begin;
WriteLn('reading '+a+'...');
if (xtOpen(t,a,true)<>0) then immErr('error opening!');
serverPort:=BVal(gnl);
logFileName:=gnl;
logFileLock:=(BVal(gnl)<>0);
xtClose(t);
End;



Label f1;
Var
  d:array[1..2*1024] of byte;
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn('syslog server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=ParamStr(1);
if (a='') then immErr('using: syslog.code <config>');
ReadUpConfig(a);
logFileDate:=0;

if (serverPort=0) then serverPort:=514;
if UDPlistenOnPort(serverPipe,65536,serverAddr,serverPort) then immErr('failed to listen on udp port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+' udp port...');

BugOS_SignDaemoning;
f1:
relequish;
o:=sizeof(d);
while not UDPreceivePacket(serverPipe,a,i,d,o) do begin;
  append2log(ipAddr2string(a)+' '+BStr(i),d,o);
  o:=sizeof(d);
  end;
goto f1;
END.