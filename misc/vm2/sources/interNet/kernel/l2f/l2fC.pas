{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc random.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Const proggyName='l2f client v1.0';

{$include l2fC.inc}


Var
  buf:array[1..1024] of byte;
  a:String;
  i,o,p:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

if string2ipAddr(ParamStr(1),prAdr) then immErr('using: l2f.code <host> [port]');
prPrt:=BVal(ParamStr(2));
if (prPrt=0) then prPrt:=1701;

i:=prPrt;
if UDPlistenOnPort(udpPipe,65536,a,i) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
WriteLn('will send to '+ipAddr2string(prAdr)+' '+BStr(prPrt)+'...');

openTunnelConn;
openSessionConn;
WaitForUpperLayer;
doOneConnection;
closeTunnelConn;
END.