{$undef debug}
{{$define debug}
{$heap 95k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc filesys.inc}
{$sysinc textfile.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}
{$sysinc datetime.inc}
{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}

Const proggyName='sip server v1.0';

{$include \sources\internet\kernel\utils\timer2.inc}
{$include memory.inc}
{$include config.inc}
{$include g711alaw.inc}
{$include sip1.inc}
{$include sip2.inc}
{$include sip3.inc}

Procedure gotIncoming(p:LongInt);
Var
  con:OneConnectionRecord;
  i,o:LongInt;
Begin;
fillchar(con,sizeof(con),0);
con.pipe:=p;
pipeLineStats(p,o,i,i);
if (o=startingProc) then begin;
  startingProc:=0;
  i:=FindOneConnectionByStat(2);
  if (i=0) then begin;
    pipeLineClose(p);
    exit;
    end;
  inc(ConnectionDat^[i].stat);
  ConnectionDat^[i].pipe:=p;
  exit;
  end;
if ResizeMem(ConnectionNum+1) then begin;
  WriteLn('failed to allocate mrmory!');
  pipeLineClose(p);
  exit;
  end;
con.addr:=serverAddr;
con.port:=serverPort;
con.time:=currentTime;
ConnectionDat^[ConnectionNum]:=con;
End;


Label f1;
Var
  a:String;
  i,o:LongInt;
  con:OneConnectionRecord;
BEGIN;
WriteLn(proggyName+', done by Mc at '#%date' '#%time'.');
if TCPfindProcess then immErr('failed to find tcp process!');
if pipeLineBegListen then immErr('failed to start listening!');
generateAlawTable;

a:=ParamStr(1);
if (a='') then immErr('using: sip.code <config>');
ReadUpConfig(a);

if (localPort=0) then localPort:=5060;
if UDPlistenOnPort(localPipe,65536,localAddr,localPort) then immErr('failed to listen on udp port!');
WriteLn('listening on '+ipAddr2string(localAddr)+' '+BStr(localPort)+' udp port...');
if (serverPort=0) then serverPort:=5060;
WriteLn('sip proxy is '+ipAddr2string(serverAddr)+' '+BStr(serverPort)+' udp port...');

timer2start;
ConnectionNum:=0;
lastRegged:=-999999;
if RTPfindProcess then immErr('failed to find rtp process!');
ticksPerPack:=ticksPerSec div 32;
if (ticksPerPack<1) then ticksPerPack:=1;

BugOS_SignDaemoning;
f1:
relequish;
timer2start;
if (getTimePast(lastRegged)>5*60) then sendRegisterMessage;
if (startingProc<>0) then if not BugOS_ProcessExists(startingProc) then startingProc:=0;
while (pipeLineGetIncoming(i)=0) do gotIncoming(i);
releq2upper;
for i:=ConnectionNum downto 1 do if releq2user(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe);
  pipeLineClose(con.strm);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;
goto f1;
END.