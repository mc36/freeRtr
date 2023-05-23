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

Const proggyName='bstun server v1.0';

{$include memory.inc}
{$include config.inc}
{$include bstun.inc}
{$include bstunS.inc}



Label f1;
Var
  a:String;
  i,o:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

a:=paramStr(1);
if (a='') then immErr('using: bstun.code <config>');
ReadUpConfig(a);

if (servPort=0) then servPort:=1976;
if TCPlistenOnPort(i,65536,servAddr,servPort) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(servAddr)+' '+BStr(servPort)+'...');

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
for i:=1 to ConnectionNum do relequishConn(ConnectionDat^[i]);
while (pipeLineGetIncoming(i)=0) do gotIncoming(i);
goto f1;
END.