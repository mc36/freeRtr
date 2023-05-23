{$heap 95k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc filesys.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}
{$include \sources\internet\server\sip\g711alaw.inc}


{$include memory.inc}
{$include skinny1.inc}
{$include skinny2.inc}
{$include skinny3.inc}
{$include skinny4.inc}





Label f1;
Var
  i,o:LongInt;
  a:String;
BEGIN;
WriteLn('skinny client v1.0, done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
generateAlawTable;

a:=paramStr(1);
if string2ipAddr(a,serverAddr) then immErr('using: skinny.code <server> <macaddr> [port] [process] [params]');
a:=paramStr(2);
kicserel(' ','',a);
kicserel('-','',a);
kicserel('.','',a);
for i:=1 to sizeof(macAddress) do begin;
  macAddress[i]:=BVal('$'+copy(a,1,2));
  a:=copy(a,3,666);
  end;
serverPort:=BVal(paramStr(3));
if (serverPort=0) then serverPort:=2000;
processNam:=paramStr(4);
processPar:='';
for i:=5 to paramCount do processPar:=processPar+' '+paramStr(i);
processPar:=copy(processPar,2,666);
BugOS_MyProcessInfo(o,i,i);
kicserel('"',BStr(o),processPar);

Write('connecting to '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+'...');
TCPbeginConnect(serverPipe,65536,serverAddr,serverPort);
while TCPlookConnected(serverPipe,localAddr,localPort,i) do begin;
  relequish;
  if (serverPipe<>0) then continue;
  immErr(' failed!');
  end;
WriteLn(' ok!');
WriteLn('local side is '+ipAddr2string(localAddr)+' '+BStr(localPort)+'...');

if RTPfindProcess then immErr('failed to find rtp process!');
upperPipe:=0;
upperStat:=0;
voicePipe:=0;
voiceMode:=0;
fillchar(lastHeader,sizeof(lastHeader),0);
myPhoneNumber:='???';
timer2start;
callProcNum:=0;
ticksPerPack:=ticksPerSec div 32;
if (ticksPerPack<1) then ticksPerPack:=1;

skinnySendUnreg;
skinnySendAlarm;
skinnySendReg;
skinnySendButtonTempReq;
skinnySendSoftKeyTempReq;
skinnySendCfgStatReq;
skinnySendSoftKeySetReq;
skinnySendDateTimeReq;
skinnySendLineStatReq;
skinnySendFwdStatReq;
skinnySendAvilLnsReq;

pipeLineBegListen;
BugOS_SignDaemoning;

f1:
relequish;
timer2start;
releq2voice;
releq2server;
releq2upper;
goto f1;
END.