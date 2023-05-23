{$define debug}
{$undef debug}
{$heap 63k}
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
{$sysinc random.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='layer2 tunnel protocol 3 v1.0';

{$include memory.inc}
{$include config.inc}
{$include l2tp3.inc}


Label f1,f2;
Var
  a:String;
  i,o:LongInt;
  d:array[1..1024*4] of byte;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
Randomize;

a:=ParamStr(1);
if (a='') then immErr('using: l2tp3.code <config>');
ReadUpConfig(a);

if (ipProto=0) then ipProto:=$73;
i:=ipProto;
if ProtoListenOnPort(pipe,65536,addr,i) then exit;
WriteLn('listening on '+ipAddr2string(addr)+' '+BStr(i)+'...');

timer2start;
for i:=1 to ConnectionNum do clearOneConn(ConnectionDat[i]);

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while (pipeLineGetIncoming(i)=0) do gotNewIncomingConnection(i);
for i:=1 to ConnectionNum do relequish2conn(ConnectionDat[i]);
for i:=1 to SessionNum do relequish2sess(SessionDat[i]);
f2:
i:=sizeof(d);
if UDPreceivePacket(pipe,a,o,d,i) then goto f1;
gotOnePacket(d,i,a);
goto f2;
END.