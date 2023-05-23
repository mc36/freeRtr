{$heap 63k}
{$stack 3k}
{$sysinc system.inc}
{$sysinc alap.inc}
{$sysinc hex.inc}
{$sysinc crt.inc}
{$sysinc param.inc}
{$sysinc bugos.inc}
{$sysinc pipeline.inc}

{$sysinc memory.inc}
{$sysinc inet_addr.inc}
{$sysinc inet_tcp.inc}
{$include \sources\internet\kernel\utils\timer2.inc}

{$include memory.inc}
{$include loadBalance.inc}


Label f1;
Var
  con:OneConnectionRecord;
  i,o,p:LongInt;
  a,b:String;
BEGIN;
WriteLn('load balancer v1.0, done by Mc at '#%date' '#%time'.');

if (paramCount<3) then immErr('using: loadbalance.code <localport> <ipaddr> <port> [<ipaddr> <port>]');
sourcePort:=BVal(ParamStr(1));
targetNum:=0;
p:=2;
while (p<paramCount) do begin;
  if string2ipAddr(ParamStr(p),a) then immErr('error in address!');
  inc(p);
  i:=BVal(ParamStr(p));
  inc(p);
  if (i=0) then i:=sourcePort;
  inc(targetNum);
  targetPort[targetNum]:=i;
  move(a,targetAddr[targetNum],sizeof(sourceAddr));
  end;
fillchar(targetStat,sizeof(targetStat),0);
targetNxt:=0;
lastTest:=0;

if TCPfindProcess then immErr('failed to find tcp process!');

if TCPlistenOnPort(p,65536,sourceAddr,sourcePort) then immErr('failed to listen on port!');
WriteLn('listening on '+ipAddr2string(sourceAddr)+' '+BStr(sourcePort)+' port...');
WriteLn('will use '+BStr(targetNum)+' alternate servers:');
for p:=1 to targetNum do WriteLn('#'+BStr(p)+': '+ipAddr2string(targetAddr[p])+' '+BStr(targetPort[p]));

ConnectionNum:=0;
if pipeLineBegListen then immErr('failed to start listening!');
WriteLn('serving others...');
BugOS_SignDaemoning;
f1:
relequish;
timer2start;
if (getTimePast(lastTest)>60) then begin;
  for i:=1 to targetNum do begin;
    if (targetStat[i]<>2) then continue;
    if (getTimePast(targetTime[i])<60*5) then continue;
    WriteLn('will give a try to #'+BStr(i));
    targetStat[i]:=1;
    end;
  lastTest:=currentTime;
  end;
while (pipeLineGetIncoming(p)=0) do begin;
  if ResizeMem(ConnectionNum+1) then begin;
    pipeLineClose(p);
    WriteLn('failed to allocate memory!');
    goto f1;
    end;
  fillchar(con,sizeof(con),0);
  con.pipe1:=p;
  con.time:=CurrentTime;
  con.stat:=1;
  ConnectionDat^[ConnectionNum]:=con;
  end;
for i:=ConnectionNum downto 1 do if doConn(ConnectionDat^[i]) then begin;
  con:=ConnectionDat^[i];
  pipeLineClose(con.pipe1);
  pipeLineClose(con.pipe2);
  move(ConnectionDat^[ConnectionNum],ConnectionDat^[i],sizeof(con));
  ResizeMem(ConnectionNum-1);
  end;

goto f1;
END.