{$undef debug}
{{$define debug}
{$heap 63k}
{$stack 63k}
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

Const proggyName='sip proxy v1.0';

{$include \sources\internet\kernel\utils\timer2.inc}
{$include memory.inc}
{$include config.inc}
{$include sip1.inc}
{$include sip2.inc}



Label f1;
Var
  a:String;
  i,o:LongInt;
  con:OneConnectionRecord;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=ParamStr(1);
if (a='') then immErr('using: siproxy.code <config>');
ReadUpConfig(a);
QuickSort(1,ConnectionNum);

if (localPort=0) then localPort:=5060;
if UDPlistenOnPort(localPipe,65536,localAddr,localPort) then immErr('failed to listen on udp port!');
WriteLn('listening on '+ipAddr2string(localAddr)+' '+BStr(localPort)+' udp port...');

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
while releq2upper do;
if (getTimePast(lastScand)<60) then goto f1;
for i:=1 to ConnectionNum do releq2user(ConnectionDat^[i]);
lastScand:=currentTime;
goto f1;
END.