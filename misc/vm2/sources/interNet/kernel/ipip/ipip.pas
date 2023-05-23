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

{$include memory.inc}
{$include config.inc}
{$include ipip.inc}


Label f1,f2,f3;
Var
  a:String;
  i,o:LongInt;
  d:array[1..1024*4] of byte;
BEGIN;
WriteLn('ip in ip v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');

a:=ParamStr(1);
if (a='') then immErr('using: ipip.code <config>');
ReadUpConfig(a);

if (ip4prot=0) then ip4prot:=$04;
i:=ip4prot;
if ProtoListenOnPort(ip4pipe,65536,ip4addr,i) then exit;
WriteLn('listening on '+ipAddr2string(ip4addr)+' '+BStr(i)+'...');

if (ip6prot=0) then ip6prot:=$29;
i:=ip6prot;
if ProtoListenOnPort(ip6pipe,65536,ip6addr,i) then exit;
WriteLn('listening on '+ipAddr2string(ip6addr)+' '+BStr(i)+'...');

BugOS_SignDaemoning;
f1:
relequish;
while (pipeLineGetIncoming(i)=0) do gotNewIncomingConnection(i);
for i:=1 to ConnectionNum do relequish2connection(ConnectionDat[i]);
f2:
i:=sizeof(d);
if not UDPreceivePacket(ip4pipe,a,o,d[4],i) then begin;
  gotOnePacket(d,i,$800,a);
  goto f2;
  end;
f3:
i:=sizeof(d);
if UDPreceivePacket(ip6pipe,a,o,d[4],i) then goto f1;
gotOnePacket(d,i,$86dd,a);
goto f3;
END.