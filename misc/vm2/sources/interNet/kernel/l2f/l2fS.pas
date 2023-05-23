{$define debug}
{$undef debug}
{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc pipeline.inc}
{$sysinc bugos.inc}
{$sysinc random.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='l2f server v1.0';

{$include memory.inc}
{$include config.inc}
{$include l2fS.inc}

Procedure gotIncoming(p:LongInt);
Var
  i,o:LongInt;
  a:String;
Begin;
o:=FindConnectByStat(3);
if (o<1) then begin;
  {$ifdef debug}
  WriteLn('refused connection from upper layer...');
  {$endif}
  pipeLineClose(p);
  exit;
  end;
ConnectionDat^[o].pipe:=p;
ConnectionDat^[o].stat:=4;
a:=ipAddr2string(ConnectionDat^[o].addr)+' '+BStr(ConnectionDat^[o].port);
a:='12341234'#0#0#0#0#0#0#0#0+chr(ConnectionDat^[o].tuneR)+#255'l2f with '+a+#0;
i:=1;move(i,a[1],sizeof(i));
i:=1400;move(i,a[5],sizeof(i));
pipeLineSend(p,a[1],length(a));
{$ifdef debug}
WriteLn('accepted connection from upper layer...');
{$endif}
End;


Label f1;
Var
  a:String;
  i,o:LongInt;
  buf:array[1..2*1024] of byte;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

a:=paramStr(1);
if (a='') then immErr('using: l2f.code <config>');
ReadUpConfig(a);

if (servPort=0) then servPort:=1701;
if UDPlistenOnPort(servPipe,65536,servAddr,servPort) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(servAddr)+' '+BStr(servPort)+'...');

pipeLineBegListen;
BugOS_SignDaemoning;
timer2start;

f1:
o:=sizeof(buf);
if not UDPreceivePacket(servPipe,a,i,buf,o) then begin;
  gotOnePacket(a,i,buf,o);
  goto f1;
  end;
for i:=1 to ConnectionNum do relequishConn(ConnectionDat^[i]);
while (pipeLineGetIncoming(i)=0) do gotIncoming(i);
relequish;
timer2start;
goto f1;
END.