{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include portForward.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('port forwarder (udp) v1.0, done by Mc at '#%date' '#%time'.');

if (paramCount<3) then immErr('using: portforward.code <localport> <ipaddr> <port>');
sourcePort:=BVal(ParamStr(1));
if string2ipAddr(ParamStr(2),targetAddr) then immErr('error in address!');
targetPort:=BVal(ParamStr(3));
if (targetPort=0) then targetPort:=sourcePort;

if TCPfindProcess then immErr('failed to find tcp process!');

if UDPlistenOnPort(sourcePipe,65536,sourceAddr,sourcePort) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(sourceAddr)+' '+BStr(sourcePort)+' port...');
WriteLn('will connect to '+ipAddr2string(targetAddr)+' '+BStr(targetPort)+' port...');

ConnectionNum:=0;
if pipeLineBegListen then immErr('failed to start listening!');
WriteLn('serving others...');
BugOS_SignDaemoning;
f1:
relequish;
timer2start;
doUpper;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.