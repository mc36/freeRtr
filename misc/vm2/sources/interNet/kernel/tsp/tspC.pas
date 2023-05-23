{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc param.inc}
{$sysinc hex.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include parseXML.inc}
{$include tspC.inc}


Label f1;
BEGIN;
WriteLn('tsp client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

if string2ipAddr(ParamStr(1),prAdr) then immErr('using: tsp.code <host> <port> <4 or 6> <user> <pass>');
prPrt:=BVal(ParamStr(2));
reqAddr:=BVal(ParamStr(3));
userPass:=#0+paramStr(4)+#0+paramStr(5);
if not (reqAddr in [4,6]) then begin;
  if isAddressIPv4mask(prAdr) then reqAddr:=6 else reqAddr:=4;
  end;
if (prPrt=0) then prPrt:=3653;

myPrt:=0;
if UDPlistenOnPort(udpPipe,65536,myAdr,myPrt) then immErr('failed to open listening!');
WriteLn('listening on '+ipAddr2string(myAdr)+' '+BStr(myPrt)+'...');
WriteLn('will send to '+ipAddr2string(prAdr)+' '+BStr(prPrt)+'...');
WriteLn('will request ipv'+BStr(reqAddr)+' address...');

openTunnelConn;
Write('waiting for upper level...');
BugOS_SignDaemoning;
pipeLineBegListen;
uprPipe:=0;
while (uprPipe=0) do releq2tunnel;
WriteLn(' done!');
f1:
releq2tunnel;
goto f1;
END.