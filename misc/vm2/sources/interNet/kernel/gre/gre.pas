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
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\checksum.inc}

{$include memory.inc}
{$include config.inc}
{$include gre.inc}


Label f1,f2;
Var
  a:String;
  i,o:LongInt;
  d:array[1..1024*4] of byte;
BEGIN;
WriteLn('generic routing encapsulation v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: gre.code <config>');
ReadUpConfig(a);

if (ipProto=0) then ipProto:=$2f;
i:=ipProto;
if ProtoListenOnPort(pipe,65536,addr,i) then exit;
WriteLn('listening on '+ipAddr2string(addr)+' '+BStr(i)+'...');

BugOS_SignDaemoning;
f1:
relequish;
while (pipeLineGetIncoming(i)=0) do gotNewIncomingConnection(i);
for i:=1 to ConnectionNum do relequish2connection(ConnectionDat[i]);
f2:
i:=sizeof(d);
if UDPreceivePacket(pipe,a,o,d,i) then goto f1;
gotOnePacket(d,i,a);
goto f2;
END.