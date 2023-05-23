{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc param.inc}
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

Const proggyName='l2tp client v1.0';

{$include l2tpC.inc}


Var
  a:String;
  i:LongInt;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
Randomize;
if TCPfindProcess then immErr('failed to find tcp process!');

cnBeg:=10;
if string2ipAddr(ParamStr(1),prAdr) then immErr('using: l2tp.code <host> [port] [phone#]');
prPrt:=BVal(ParamStr(2));
cnNum:=ParamStr(3);
if (prPrt=0) then prPrt:=1701;
if (cnNum<>'') then cnBeg:=7;

i:=0;
if UDPlistenOnPort(udpPipe,65536,a,i) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(a)+' '+BStr(i)+'...');
WriteLn('will send to '+ipAddr2string(prAdr)+' '+BStr(prPrt)+'...');

openTunnelConn;
openSessionConn;
WaitForUpperLayer;
doOneConnection;
closeTunnelConn;
END.