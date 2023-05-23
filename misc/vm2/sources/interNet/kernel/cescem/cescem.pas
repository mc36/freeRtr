{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

Procedure immErr(a:String);
Begin;
WriteLn(a);
Halt(1);
End;

{$include memory.inc}
{$include config.inc}
{$include cescem.inc}


Label f1;
Var
  buf:array[1..1024*4] of byte;
  con:OneConnectionRecord;
  adr:OneTCPaddressRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('cescem server v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');

a:=ParamStr(1);
if (a='') then immErr('using: cescem.code <config>');
ReadUpConfig(a);

i:=serverPort;
if (i=0) then i:=2142;
if UDPlistenOnPort(serverPipe,65536,serverAddr,i) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(serverAddr)+' '+BStr(i)+' port...');
serverPort:=i;
if (remotePort=0) then remotePort:=serverPort;
WriteLn('will send to '+ipAddr2string(remoteAddr)+' '+BStr(remotePort)+' port...');

timer2start;
bytePerTick:=8000 div ticksPerSec;
if (bytePerTick<1) then bytePerTick:=1;
bytePerTick:=bytePerTick*ConnectionNum;
Write('channels='+BStr(ConnectionNum)+' groups='+BStr(GroupNumber)+' packet='+BStr(packetSize)+' groups=');
for i:=1 to GroupNumber do Write(BStr(ConnectionDat^[i].min)+'-'+BStr(ConnectionDat^[i].max)+' ');
WriteLn('');

lastGotTime:=currentTime;
lastGotPack:=-1;
lastSentTime:=currentTime;
lastPackSeq:=0;
lastPackTim:=0;
lastPackSiz:=0;

BugOS_SignDaemoning;
pipeLineBegListen;
f1:
relequish;
timer2start;
doUpper;
doLower;
doSender;
goto f1;
END.